{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

data HealthValue = OK | NOK deriving (Eq, Show, Generic)

instance ToJSON HealthValue
instance FromJSON HealthValue

data HealthStatus = HealthStatus {
  health :: HealthValue
} deriving (Eq, Show, Generic)

instance ToJSON HealthStatus
instance FromJSON HealthStatus


type HealthcheckApi =
  "healthcheck" :> Get '[JSON] HealthStatus

healthcheckApi :: Proxy HealthcheckApi
healthcheckApi = Proxy


main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp


mkApp :: IO Application
mkApp = return $ serve healthcheckApi server

server :: Server HealthcheckApi
server =
  getHealthcheck

getHealthcheck :: Handler HealthStatus
getHealthcheck = return $ HealthStatus OK
