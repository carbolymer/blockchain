{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Service.Server
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The implementation of `BlockchainService` interface - the server part
--
-----------------------------------------------------------------------------

module Blockchain.Service.Server (newBlockchainServiceHandle) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Either (rights)
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Time (getCurrentTime)

import Blockchain.Config (BlockchainConfig)
import Blockchain.Core ((<$$>), Block(..), Blockchain(..), addNodes, addTransaction, evalApp,
    runApp, mineNewBlock, validateAndUpdateChain)
import Blockchain.Service (BlockchainService(..), HealthStatus(..), HealthCheck(..), StatusMessage(..))
import Blockchain.RestApi.Client (NodeRequest(..), RequestException, newBlockchainRestApiClient, runRequests)
import Logger

infoL, warnL :: (MonadIO m) => String -> m ()
[infoL, warnL] = getLogger "Blockchain.Service.Server" [INFO, WARNING]

-- | Creates new `BlockchainService` handle
newBlockchainServiceHandle :: BlockchainConfig -> Blockchain -> IO (BlockchainService IO)
newBlockchainServiceHandle cfg blockchain = return $ BlockchainService {
    getHealthCheck = return $ HealthCheck OK,

    newTransaction = \transaction -> do
      _ <- runApp cfg (addTransaction transaction) blockchain
      return $ StatusMessage "Transaction was addedd",

    getConfirmedTransactions = concat <$> (map transactions) <$$> readTVarIO $ blocks blockchain,

    getUnconfirmedTransactions = readTVarIO $ currentTransactions blockchain,

    mineBlock = do
      currentTime <- getCurrentTime
      evalApp cfg (mineNewBlock currentTime) blockchain,

    getBlockchain = readTVarIO $ blocks blockchain,

    getNodes = S.toList <$$> readTVarIO $ nodes blockchain,

    registerNodes = \nodes -> do
      _ <- runApp cfg (addNodes nodes) blockchain
      return $ StatusMessage "Nodes addes to nodes list",

    resolveNodes = do
      nodesList <- S.toList <$$> readTVarIO $ nodes blockchain
      let requestsList = map (\node -> NodeRequest node (getBlockchain newBlockchainRestApiClient)) nodesList
      wasChainUpdated <- evalApp cfg (fetchChainsAndResolve requestsList) blockchain
      let message = if wasChainUpdated then "Longer chain found. Current chain was updated."
                                       else "We have longest chain. Left intact."
      infoL message
      return $ StatusMessage (fromString message)
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
