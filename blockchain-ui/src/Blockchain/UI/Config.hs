-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.UI.Config
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The settings of the application
--
-----------------------------------------------------------------------------

module Blockchain.UI.Config (
  UiConfig(..),
  defaultConfig
) where

import Data.Text (Text)

data UiConfig = UiConfig {
  httpPort :: !Int,
  staticFilesPath :: !Text,
  beaconNodeUrl :: !Text,
  nodesListRefreshInterval :: !Int
} deriving (Eq, Show)


defaultConfig :: UiConfig
defaultConfig = UiConfig {
  httpPort = 8000,
  staticFilesPath = "blockchain-ui-fe/dist",
  beaconNodeUrl = "http://beacon-node:8000/",
  nodesListRefreshInterval = 400*1000 -- 400 ms
}
