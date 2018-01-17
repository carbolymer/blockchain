-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.NodesNetwork
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Abstraction layer for communication between nodes
--
-----------------------------------------------------------------------------

module Blockchain.Node.NodesNetwork (
    NodesNetworkService(..)

  , newHandle
) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Either (lefts, rights)
import           Data.List (nub)
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S

import           Blockchain.Node.Config (BlockchainConfig(..))
import           Blockchain.Node.Core ((<$$>), NodeState(..), Node(..), thisNode)
import           Blockchain.Node.RestApi.Client (NodeRequest(..), newBlockchainRestApiClient, runRequests)
import           Blockchain.Node.Service (BlockchainService(..), MessageLevel(..), StatusMessage(..))
import qualified Logger

infoL, errorL :: (MonadIO m) => String -> m ()
[infoL, errorL] = Logger.getLogger "Blockchain.Node.NodesNetwork" [Logger.INFO, Logger.ERROR]

data NodesNetworkService = NodesNetworkService {
    registerToBeacon :: IO StatusMessage,
    runBackgroundNodesResolver :: IO (),
    runBackgroundNodesListPropagator :: IO ()
  }


isCurrentNodeABeacon :: BlockchainConfig -> NodeState -> Bool
isCurrentNodeABeacon cfg nodeState
    = (url $ thisNode cfg nodeState) == beaconNodeUrl cfg


newHandle :: NodeState
          -> IO NodesNetworkService
newHandle nodeState = do
  let cfg = config nodeState
  return $ NodesNetworkService {
    registerToBeacon = if isCurrentNodeABeacon cfg nodeState
      then
        return $ StatusMessage INFO "Current node is a beacon"
      else do
        let nodeRequest = NodeRequest
              (Node "" (beaconNodeUrl cfg))
              (registerNodes newBlockchainRestApiClient [thisNode cfg nodeState])

        result <- listToMaybe <$> runRequests [nodeRequest]

        case result of
          Just (Left e) -> do
              errorL (show e)
              return $ StatusMessage ERROR "Registering in beacon node failed"
          Just (Right response) -> do
              infoL (show response)
              return response
          _ -> do
              errorL "Did not receive message from beacon node!"
              return $ StatusMessage ERROR "Registering in beacon node failed",

    runBackgroundNodesResolver = void $ forkIO $ forever $ do
      -- just run on itself
      let targetNodes = [thisNode cfg nodeState]
      let nodeRequests = map (\node -> NodeRequest
              node
              (resolveNodes newBlockchainRestApiClient))
            targetNodes

      -- result is already logged in the BlockchainService, we don't need to handle it
      _ <- runRequests nodeRequests

      threadDelay $ consensusInterval cfg,

    runBackgroundNodesListPropagator = void $ forkIO $ forever $ do
      let thisNode' = thisNode cfg nodeState
      otherNodes <- S.toList <$$> readTVarIO $ nodes nodeState
      let nodesToSend = nub $ [thisNode'] ++ otherNodes
      let nodeRequests = map (\node -> NodeRequest
              node
              (registerNodes newBlockchainRestApiClient nodesToSend))
            otherNodes

      result <- runRequests nodeRequests

      mapM_ (errorL . show) (lefts result)
      mapM_ (infoL . show) (rights result)

      threadDelay $ nodesListRegisterInterval cfg
  }
