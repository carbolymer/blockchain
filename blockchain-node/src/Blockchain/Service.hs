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

-- | Value of node health
data HealthStatus = OK  -- ^ All systems running
                  | NOK -- ^ There are some issues
                  deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus

-- | DTO for `HealthStatus`
data HealthCheck = HealthCheck {
  health :: HealthStatus    -- ^ Returns health status
} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck
instance FromJSON HealthCheck

-- | DTO with status message
newtype StatusMessage = StatusMessage {
  message :: Text   -- ^ The status message
} deriving (Eq, Show, Generic)

instance ToJSON StatusMessage
instance FromJSON StatusMessage

-- | A blockchain main service
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
