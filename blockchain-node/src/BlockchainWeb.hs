{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  BlockchainWeb
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

module BlockchainWeb (
    BlockchainWebService(..)
  , HealthCheck(..)
  , HealthStatus(..)
  , StatusMessage
  , newBlockchainWebServiceHandle
) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Time (getCurrentTime)
import           GHC.Generics (Generic)

import BlockchainConfig (BlockchainConfig)
import Blockchain ((<$$>), Block(..), Blockchain(..), Node(..), Transaction(..), addNodes, addTransaction, evalApp,
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

data BlockchainWebService = BlockchainWebService {
    getHealthCheck :: IO HealthCheck
  , newTransaction :: Transaction -> IO StatusMessage
  , getConfirmedTransactions :: IO [Transaction]
  , getUnconfirmedTransactions :: IO [Transaction]
  , mineBlock :: IO Block
  , getBlockchain :: IO [Block]
  , getNodes :: IO [Node]
  , registerNodes :: [Node] -> IO StatusMessage
  , resolveNodes :: IO StatusMessage
}

newBlockchainWebServiceHandle :: BlockchainConfig -> Blockchain -> IO BlockchainWebService
newBlockchainWebServiceHandle cfg blockchain = do

  return BlockchainWebService {
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

    resolveNodes = return $ StatusMessage "not implemented"
  }
