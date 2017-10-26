-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Config
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The settings of the application
--
-----------------------------------------------------------------------------

module Blockchain.Config where

-- | The settings data structure
data BlockchainConfig = BlockchainConfig {
  httpPort :: !Int,          -- ^ HTTP listening port
  miningDifficulty :: !Int,  -- ^ mining difficulty
  miningReward :: !Int    -- ^ Reward for mining a block
} deriving (Eq, Show)

-- | Default config for the application
--
-- > httpPort = 8000
-- > miningDifficulty = 5
-- > miningReward = 1
--
defaultConfig :: BlockchainConfig
defaultConfig = BlockchainConfig {
  httpPort = 8000,
  miningDifficulty = 5,
  miningReward = 1
}
