{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.RestApi
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Defines REST API for the node. The server part
--
-----------------------------------------------------------------------------
module Blockchain.RestApi.Server (
  bootstrap
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function((&))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:>), (:<|>)(..), Application, Get, Handler, JSON, NoContent(..), Post, Proxy(..), ReqBody, Server, serve)

import Blockchain.Core (Block, Node, Transaction, newBlockchain)
import Blockchain.Config (BlockchainConfig(..), defaultConfig)
import Blockchain.Service (BlockchainService(..), HealthCheck(..), StatusMessage, newBlockchainServiceHandle)
import Blockchain.RestApi (RestApi, restApi)
import Logger


[infoL] = getLogger "Blockchain.RestApi.Server" [INFO]


server :: BlockchainService IO -> Server RestApi
server service = toApi getHealthCheck
        :<|> toApi newTransaction
        :<|> toApi getConfirmedTransactions
        :<|> toApi getUnconfirmedTransactions
        :<|> toApi mineBlock
        :<|> toApi getBlockchain
        :<|> toApi getNodes
        :<|> toApi registerNodes
        :<|> toApi resolveNodes
            where
              toApi method = convert (service & method)


-- | Starts the web application
bootstrap :: BlockchainConfig -> IO ()
bootstrap config = do
  let settings =
        setPort (httpPort config) $
        setBeforeMainLoop (infoL $ "listening on port " ++ show (httpPort config)) $
        defaultSettings
  runSettings settings =<< (makeApplication config)


makeApplication :: BlockchainConfig -> IO Application
makeApplication config = do
  bws <- blockchainService
  return $ serve restApi (server bws)
  where
    blockchainService :: IO (BlockchainService IO)
    blockchainService = newBlockchain >>= (newBlockchainServiceHandle config)


class ConvertibleToApiHandler s t where
  convert :: s -> t

instance ConvertibleToApiHandler (IO s) (Handler s) where
  convert = liftIO

instance ConvertibleToApiHandler (IO ()) (Handler NoContent) where
  convert _ = return NoContent

instance (ConvertibleToApiHandler s t) => ConvertibleToApiHandler (a -> s) (a -> t) where
  convert function = convert . function
