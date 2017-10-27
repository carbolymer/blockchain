{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Service.Server (newBlockchainServiceHandle) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Set as S
import           Data.Time (getCurrentTime)

import Blockchain.Config (BlockchainConfig)
import Blockchain.Core ((<$$>), Block(..), Blockchain(..), Node(..), Transaction(..), addNodes, addTransaction, evalApp,
    runApp, mineNewBlock)
import Blockchain.Service (BlockchainService(..), HealthStatus(..), HealthCheck(..), StatusMessage(..))


newBlockchainServiceHandle :: BlockchainConfig -> Blockchain -> IO (BlockchainService IO)
newBlockchainServiceHandle cfg blockchain = return $ BlockchainService {
    getHealthCheck = return $ HealthCheck OK,

    newTransaction = \transaction -> do
      runApp cfg (addTransaction transaction) blockchain
      return $ StatusMessage "Transaction was addedd",

    getConfirmedTransactions = concat <$> (map transactions) <$$> readTVarIO $ blocks blockchain,

    getUnconfirmedTransactions = readTVarIO $ currentTransactions blockchain,

    mineBlock = do
      currentTime <- getCurrentTime
      evalApp cfg (mineNewBlock currentTime) blockchain,

    getBlockchain = readTVarIO $ blocks blockchain,

    getNodes = S.toList <$$> readTVarIO $ nodes blockchain,

    registerNodes = \nodes -> do
      runApp cfg (addNodes nodes) blockchain
      return $ StatusMessage "Nodes addes to nodes list",

    resolveNodes = do

      return $ StatusMessage "not implemented"
  }
