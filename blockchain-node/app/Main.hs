{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:>), Application, Get, Handler, JSON, Proxy(..), Server, serve)
import System.IO (hPutStrLn, stderr)

import BlockchainWeb (HealthCheck(..), HealthStatus(..), getHealthCheck)

main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< makeApplication


makeApplication :: IO Application
makeApplication = return $ serve healthcheckApi server

server :: Server HealthcheckApi
server = liftIO getHealthCheck

--
-- Healthcheck
--
type HealthcheckApi =
  "healthcheck" :> Get '[JSON] HealthCheck

healthcheckApi :: Proxy HealthcheckApi
healthcheckApi = Proxy
