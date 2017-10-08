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
    HealthCheck(..)
  , HealthStatus(..)
  , getHealthCheck
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data HealthStatus = OK | NOK deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus

data HealthCheck = HealthCheck {
  health :: HealthStatus
} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck
instance FromJSON HealthCheck

getHealthCheck :: IO HealthCheck
getHealthCheck = return $ HealthCheck OK
