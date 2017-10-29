module Main where

import System.Environment (getArgs)

import Blockchain.RestApi.Server (bootstrap)
import Blockchain.Config (BlockchainConfig(..), defaultConfig)

config :: BlockchainConfig
config = defaultConfig

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse []    = bootstrap config
parse (x:_) = bootstrap config {httpPort = read x :: Int}
