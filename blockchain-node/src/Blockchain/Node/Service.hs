--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

module Blockchain.Node.Service (
    BlockchainService(..)
  , HealthCheck(..)
  , HealthStatus(..)
  , MessageLevel(..)
  , StatusMessage(..)
) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Blockchain.Node.Core (Block, Node)
import           Blockchain.Node.Transaction (NewTransactionRequest, Transaction)

-- | Value of node health
data HealthStatus = OK  -- ^ All systems running
                  | NOK -- ^ There are some issues
                  deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus

-- | DTO for `HealthStatus`
data HealthCheck = HealthCheck {
  health :: !HealthStatus    -- ^ Returns health status
} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck
instance FromJSON HealthCheck

-- | Represents priority level of the status message
data MessageLevel = INFO    -- ^ Information
                  | ERROR   -- ^ Error message
                  deriving (Eq, Show, Generic)

instance ToJSON MessageLevel
instance FromJSON MessageLevel


-- | DTO with status message
data StatusMessage = StatusMessage {
  level   :: !MessageLevel, -- ^ Message level
  message :: !Text          -- ^ The status message
} deriving (Eq, Show, Generic)

instance ToJSON StatusMessage
instance FromJSON StatusMessage

-- | A blockchain main service
data BlockchainService m = BlockchainService {
    getHealthCheck :: m HealthCheck
  , newTransaction :: NewTransactionRequest -> m StatusMessage
  , getConfirmedTransactions :: m [Transaction]
  , getUnconfirmedTransactions :: m [Transaction]
  , mineBlock :: m (Maybe Block)
  , getBlockchain :: m [Block]
  , getNodes :: m [Node]
  , registerNodes :: [Node] -> m StatusMessage
  , resolveNodes :: m StatusMessage
}
