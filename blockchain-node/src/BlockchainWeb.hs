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
  , Node(..) -- temporary
  , StatusMessage
  , newBlockchainWebServiceHandle
) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)

import BlockchainConfig (BlockchainConfig)
import Blockchain (Block(..), Blockchain(..), Transaction(..), addTransaction, evalApp, runApp, mineNewBlock)


data HealthStatus = OK | NOK deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus

data HealthCheck = HealthCheck {
  health :: HealthStatus
} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck
instance FromJSON HealthCheck

data Node = Node deriving (Show, Eq, Generic)

instance ToJSON Node
instance FromJSON Node

newtype StatusMessage = StatusMessage {
  message :: Text
} deriving (Eq, Show, Generic)

instance ToJSON StatusMessage
instance FromJSON StatusMessage

data BlockchainWebService = BlockchainWebService {
    getHealthCheck :: IO HealthCheck
  , newTransaction :: Transaction -> IO StatusMessage
  , mineBlock :: IO Block
  , getBlockchain :: IO [Block]
  , registerNodes :: [Node] -> IO ()
  , resolveNodes :: IO ()
}

newBlockchainWebServiceHandle :: BlockchainConfig -> Blockchain -> IO BlockchainWebService
newBlockchainWebServiceHandle cfg blockchain = do

  return BlockchainWebService {
    getHealthCheck = return $ HealthCheck OK,

    newTransaction = \transaction -> do
      runApp cfg (addTransaction transaction) blockchain
      return $ StatusMessage "Transaction was addedd",

    mineBlock = do
      currentTime <- getCurrentTime
      evalApp cfg (mineNewBlock currentTime) blockchain,

    getBlockchain = readTVarIO $ blocks blockchain,

    registerNodes = \_ -> return (),

    resolveNodes = return ()
  }

