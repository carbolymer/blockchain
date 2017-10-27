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
  , StatusMessage(..)
) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import Blockchain.Core (Block, Blockchain, Node, Transaction)


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
