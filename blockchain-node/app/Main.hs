{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:>), (:<|>)(..), Application, Get, Handler, JSON, NoContent(..), Post, Proxy(..), ReqBody, Server, serve)

import Blockchain (Block, Node, Transaction, newBlockchain)
import BlockchainConfig (BlockchainConfig(..), defaultConfig)
import BlockchainWeb (BlockchainWebService(..), HealthCheck(..), StatusMessage, newBlockchainWebServiceHandle)
import Logger (getLogger)

infoL :: String -> IO ()
(infoL, _) = getLogger "Main"

config :: BlockchainConfig
config = defaultConfig

blockchainWebService :: IO BlockchainWebService
blockchainWebService = newBlockchain >>= (newBlockchainWebServiceHandle config)

main :: IO ()
main = do
  let settings =
        setPort (httpPort config) $
        setBeforeMainLoop (infoL $ "listening on port " ++ show (httpPort config)) $
        defaultSettings
  runSettings settings =<< makeApplication


makeApplication :: IO Application
makeApplication = do
  bws <- blockchainWebService
  return $ serve httpApi (server bws)


httpApi :: Proxy HttpApi
httpApi = Proxy


type HttpApi = "healthcheck" :> Get '[JSON] HealthCheck
          -- new transaction
          :<|> "transactions" :> "new" :> ReqBody '[JSON] Transaction :>  Post '[JSON] StatusMessage
          -- list of confirmed transactions in the chain
          :<|> "transactions" :> "confirmed" :> Get '[JSON] [Transaction]
          -- list of not confirmed transactions
          :<|> "transactions" :> "unconfirmed" :>  Get '[JSON] [Transaction]
          -- mines a new block
          :<|> "mine" :> Post '[JSON] Block
          -- returns whole blochchain
          :<|> "chain" :> Get '[JSON] [Block]
          -- returns a list of nodes known to this one
          :<|> "nodes" :> "list" :> Get '[JSON] [Node]
          -- accepts a list of new nodes
          :<|> "nodes" :> "register" :> ReqBody '[JSON] [Node] :> Post '[JSON] StatusMessage
          -- checks and sets the correct chain in the current node
          :<|> "nodes" :> "resolve" :> Post '[JSON] StatusMessage


server :: BlockchainWebService -> Server HttpApi
server bws = toApi getHealthCheck
        :<|> toApi newTransaction
        :<|> toApi getConfirmedTransactions
        :<|> toApi getUnconfirmedTransactions
        :<|> toApi mineBlock
        :<|> toApi getBlockchain
        :<|> toApi getNodes
        :<|> toApi registerNodes
        :<|> toApi resolveNodes
            where
              toApi method = convert (bws & method)


class ConvertibleToApiHandler s t where
  convert :: s -> t

instance ConvertibleToApiHandler (IO s) (Handler s) where
  convert = liftIO

instance ConvertibleToApiHandler (IO ()) (Handler NoContent) where
  convert _ = return NoContent

instance (ConvertibleToApiHandler s t) => ConvertibleToApiHandler (a -> s) (a -> t) where
  convert function = convert . function

