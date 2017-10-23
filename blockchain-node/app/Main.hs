module Main where

import Blockchain.RestApi.Server (bootstrap)
import Blockchain.Config (BlockchainConfig, defaultConfig)

config :: BlockchainConfig
config = defaultConfig

main :: IO ()
main = bootstrap config
