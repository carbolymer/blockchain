{-# LANGUAGE OverloadedStrings #-}
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

module Blockchain.Node.Service.Server (newBlockchainServiceHandle) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Either (rights)
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Time (getCurrentTime)

import Blockchain.Node.Config (BlockchainConfig(..))
import Blockchain.Node.Core ((<$$>), Block(..), Blockchain(..), addNodes, addTransaction, evalApp,
    runApp, mineNewBlock, thisNode, validateAndUpdateChain)
import Blockchain.Node.NodesNetwork (NodesNetworkService(..))
import Blockchain.Node.Service (BlockchainService(..), HealthStatus(..), HealthCheck(..), MessageLevel(..), StatusMessage(..))
import Blockchain.Node.RestApi.Client (NodeRequest(..), RequestException, newBlockchainRestApiClient, runRequests)
import qualified Logger

infoL, warnL :: (MonadIO m) => String -> m ()
[infoL, warnL] = Logger.getLogger "Blockchain.Node.Service.Server" [Logger.INFO, Logger.WARNING]

-- | Creates new `BlockchainService` handle
newBlockchainServiceHandle :: BlockchainConfig
                           -> Blockchain
                           -> NodesNetworkService
                           -> IO (BlockchainService IO)
newBlockchainServiceHandle cfg blockchain nodesNetworkService = do
  -- we can do it in the background
  _ <- forkIO $ do
    statusMessage <- registerToBeacon nodesNetworkService
    infoL $ show statusMessage
  runBackgroundNodesResolver nodesNetworkService
  runBackgroundNodesListPropagator nodesNetworkService

  return $ BlockchainService {
    getHealthCheck = return $ HealthCheck OK,

    newTransaction = \transaction -> do
      _ <- runApp cfg (addTransaction transaction) blockchain
      return $ StatusMessage INFO "Transaction was addedd",

    getConfirmedTransactions = concat <$> (map transactions) <$$> readTVarIO $ blocks blockchain,

    getUnconfirmedTransactions = readTVarIO $ currentTransactions blockchain,

    mineBlock = do
      currentTime <- getCurrentTime
      evalApp cfg (mineNewBlock currentTime) blockchain,

    getBlockchain = readTVarIO $ blocks blockchain,

    getNodes = do
      let thisNode' = thisNode cfg blockchain
      ([thisNode'] ++) <$> S.toList <$$> readTVarIO $ nodes blockchain,

    registerNodes = \nodes -> do
      _ <- runApp cfg (addNodes nodes) blockchain
      return $ StatusMessage INFO "Nodes addes to nodes list",

    resolveNodes = do
      nodesList <- S.toList <$$> readTVarIO $ nodes blockchain
      let requestsList = map (\node -> NodeRequest node (getBlockchain newBlockchainRestApiClient)) nodesList
      wasChainUpdated <- evalApp cfg (fetchChainsAndResolve requestsList) blockchain
      let message = if wasChainUpdated then "Longer chain found. Current chain was updated."
                                       else "We have longest chain. Left intact."
      infoL message
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
      validateAndUpdateChain (rights result)
