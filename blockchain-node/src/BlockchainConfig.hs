-----------------------------------------------------------------------------
-- |
-- Module      :  BlockchainConfig
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The settings of the application
--
-----------------------------------------------------------------------------

module BlockchainConfig where

-- | The settings data structure
data BlockchainConfig = BlockchainConfig {
  httpPort :: Int,        -- ^ HTTP listening port
  miningDifficulty :: Int -- ^ mining difficulty
} deriving (Eq, Show)

-- | Default config for the application
--
-- > httpPort = 8000
-- > miningDifficulty = 5
--
defaultConfig :: BlockchainConfig
defaultConfig = BlockchainConfig {
  httpPort = 8000,
  miningDifficulty = 5
}
