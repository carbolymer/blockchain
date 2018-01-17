-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Config
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The settings of the application
--
-----------------------------------------------------------------------------

module Blockchain.Node.Config where

import Data.Text (Text)

-- | The settings data structure
data BlockchainConfig = BlockchainConfig {
  httpPort :: !Int,                 -- ^ HTTP listening port
  miningDifficulty :: !Int,         -- ^ mining difficulty
  miningReward :: !Int,             -- ^ Reward for mining a block
  beaconNodeUrl :: !Text,           -- ^ Beacon node URL
  consensusInterval :: !Int,        -- ^ Blockchain resolution interval
  nodesListRegisterInterval :: !Int -- ^ Nodes list push interval

} deriving (Eq, Show)

-- | Default config for the application
--
-- > httpPort = 8000
-- > miningDifficulty = 5
-- > miningReward = 1
-- > beaconNodeUrl = "http://beacon-node:8000/"
-- > consensusInterval = 1000*1000 -- 1 s
-- > nodesListRegisterInterval = 2000*1000 -- 2 s
--
defaultConfig :: BlockchainConfig
defaultConfig = BlockchainConfig {
  httpPort = 8000,
  miningDifficulty = 4,
  miningReward = 1,
  beaconNodeUrl = "http://beacon-node:8000/",
  consensusInterval = 1000*1000, -- 1 s
  nodesListRegisterInterval = 2000*1000 -- 2 s
}
