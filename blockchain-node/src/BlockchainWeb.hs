-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TypeOperators #-}

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
  , newBlockchainWebServiceHandle
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Blockchain (Blockchain(..), Transaction(..))

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

data BlockchainWebService = BlockchainWebService {
    getHealthCheck :: IO HealthCheck
  , newTransaction :: Transaction -> IO ()
  , mineBlock :: IO ()
  , getBlockchain :: IO Blockchain
  , registerNodes :: [Node] -> IO ()
  , resolveNodes :: IO ()
}

newBlockchainWebServiceHandle :: Blockchain -> IO BlockchainWebService
newBlockchainWebServiceHandle blockchain = do
  return BlockchainWebService {
    getHealthCheck = return $ HealthCheck OK,
    newTransaction = \transaction -> return (),
    mineBlock = return (),
    getBlockchain = return blockchain,
    registerNodes = \_ -> return (),
    resolveNodes = return ()
  }
