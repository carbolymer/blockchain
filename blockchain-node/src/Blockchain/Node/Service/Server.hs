-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Service.Server
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The implementation of `BlockchainService` interface - the server part
--
-----------------------------------------------------------------------------

module Blockchain.Node.Service.Server (newHandle) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Either (rights)
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Text (pack)
import           Data.Time (getCurrentTime)

import           Blockchain.Node.Core ((<$$>), Block(..), NodeState(..))
import qualified Blockchain.Node.Core as Core
import qualified Blockchain.Node.Transaction as Transaction
import           Blockchain.Node.NodesNetwork (NodesNetworkService(..))
import           Blockchain.Node.Service (BlockchainService(..), HealthStatus(..),
                  HealthCheck(..), MessageLevel(..), StatusMessage(..))
import           Blockchain.Node.RestApi.Client (NodeRequest(..), RequestException,
                  newBlockchainRestApiClient, runRequests)
import           Blockchain.Node.MemPool (unMemPool)
import qualified Logger

infoL, warnL :: (MonadIO m) => String -> m ()
[infoL, warnL] = Logger.getLogger "Blockchain.Node.Service.Server" [Logger.INFO, Logger.WARNING]

-- | Creates new `BlockchainService` handle
newHandle :: NodeState
          -> NodesNetworkService
          -> IO (BlockchainService IO)
newHandle nodeState nodesNetworkService = do
  let cfg = config nodeState
  -- we can do it in the background
  _ <- forkIO $ do
    statusMessage <- registerToBeacon nodesNetworkService
    infoL $ show statusMessage
  runBackgroundNodesResolver nodesNetworkService
  runBackgroundNodesListPropagator nodesNetworkService

  return $ BlockchainService {
    getHealthCheck = return $ HealthCheck OK,

    newTransaction = \txRequest -> do
      if Transaction.newSender txRequest == Core.uuid nodeState
      then do currentTime <- getCurrentTime
              _ <- Core.runApp nodeState $ do
                Core.newTransaction
                  (Transaction.newRecipient txRequest)
                  (Transaction.newAmount txRequest)
                  currentTime
              return $ StatusMessage INFO "Transaction was addedd"
      else return $ StatusMessage ERROR $ pack $
        "Incorrect sender address: " ++ (show $ Transaction.newSender txRequest),

    getConfirmedTransactions = Core.runApp
      nodeState Core.getConfirmedTransactions,

    getUnconfirmedTransactions = unMemPool <$> (readTVarIO $ memPool nodeState),

    mineBlock = do
      currentTime <- getCurrentTime
      Core.runApp nodeState (Core.mineNewBlock currentTime),

    getBlockchain = readTVarIO $ blocks nodeState,

    getNodes = do
      let thisNode' = Core.thisNode cfg nodeState
      ([thisNode'] ++) <$> S.toList <$$> readTVarIO $ nodes nodeState,

    registerNodes = \nodes -> do
      _ <- Core.runApp nodeState (Core.addNodes nodes)
      return $ StatusMessage INFO "Nodes addes to nodes list",

    resolveNodes = do
      -- TODO add mempool propagation
      nodesList <- S.toList <$$> readTVarIO $ nodes nodeState
      let getBlockchainRequests = map (\node ->
            NodeRequest node (getBlockchain newBlockchainRestApiClient)) nodesList
      wasChainUpdated <- Core.runApp nodeState (fetchChainsAndResolve getBlockchainRequests)
      let message = if wasChainUpdated then "Longer chain found. Current chain was updated."
                                       else "We have longest chain. Left intact."
--       infoL message
      return $ StatusMessage INFO (fromString message)
  }
  where
    logNodeRequestResult :: (MonadIO m, Show a) => Either RequestException a -> m ()
    logNodeRequestResult result = case result of
      Left exception  -> warnL $ "Node response: " ++ (show exception)
      Right result    -> infoL $ "Node response: " ++ (show result)

    fetchChainsAndResolve requestsList = do
      result <- runRequests requestsList
      mapM_ logNodeRequestResult result
      Core.validateAndUpdateChain (rights result)
