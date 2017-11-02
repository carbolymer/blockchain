module Main where

import Data.Text (pack)
import System.Environment (getArgs)

import Blockchain.UI.RestApi.Server (bootstrap)
import Blockchain.UI.Config (UiConfig(..), defaultConfig)

config :: UiConfig
config = defaultConfig

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse []    = bootstrap config
parse (a1:[]) = bootstrap config {httpPort = read a1 :: Int}
parse (a1:a2:[]) = bootstrap config {
    httpPort = read a1 :: Int,
    beaconNodeUrl = pack a2
  }
parse (a1:a2:a3:_) = bootstrap config {
    httpPort = read a1 :: Int,
    beaconNodeUrl = pack a2,
    staticFilesPath = pack a3
  }
