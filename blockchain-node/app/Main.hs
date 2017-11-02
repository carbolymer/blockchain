module Main where

import System.Environment (getArgs)

import Blockchain.Node.RestApi.Server (bootstrap)
import Blockchain.Node.Config (BlockchainConfig(..), defaultConfig)

config :: BlockchainConfig
config = defaultConfig

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse []    = bootstrap config
parse (x:_) = bootstrap config {httpPort = read x :: Int}
