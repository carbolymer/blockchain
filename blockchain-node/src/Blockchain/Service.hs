{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Service
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Functions and data types adapting Blockchain module to the HTTP API.
--
-----------------------------------------------------------------------------

module Blockchain.Service (
    BlockchainService(..)
  , HealthCheck(..)
  , HealthStatus(..)
  , StatusMessage
  , newBlockchainServiceHandle
) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Time (getCurrentTime)
import           GHC.Generics (Generic)

import Blockchain.Config (BlockchainConfig)
import Blockchain.Core ((<$$>), Block(..), Blockchain(..), Node(..), Transaction(..), addNodes, addTransaction, evalApp,
    runApp, mineNewBlock)


data HealthStatus = OK | NOK deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus

data HealthCheck = HealthCheck {
  health :: HealthStatus
} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck
instance FromJSON HealthCheck

newtype StatusMessage = StatusMessage {
  message :: Text
} deriving (Eq, Show, Generic)

instance ToJSON StatusMessage
instance FromJSON StatusMessage

data (MonadIO m) => BlockchainService m = BlockchainService {
    getHealthCheck :: m HealthCheck
  , newTransaction :: Transaction -> m StatusMessage
  , getConfirmedTransactions :: m [Transaction]
  , getUnconfirmedTransactions :: m [Transaction]
  , mineBlock :: m (Maybe Block)
  , getBlockchain :: m [Block]
  , getNodes :: m [Node]
  , registerNodes :: [Node] -> m StatusMessage
  , resolveNodes :: m StatusMessage
}

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
