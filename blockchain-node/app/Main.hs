{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execState)
import Data.Function ((&))
import Data.Time (getCurrentTime)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:>), (:<|>)(..), Application, Get, Handler, JSON, NoContent(..), Post, Proxy(..), ReqBody, Server, serve)
import System.IO (hPutStrLn, stderr)

import Blockchain (Blockchain(..), Transaction(..), addNewBlock, calculateHash, calculateProofOfWork, newBlockchain)
import qualified Blockchain as B (newTransaction)
import BlockchainWeb (BlockchainWebService(..), HealthCheck(..), Node, newBlockchainWebServiceHandle)


myBlockchain :: IO Blockchain
myBlockchain = do
  currentTime <- getCurrentTime
  let blockchain = execState (B.newTransaction "sender" "recipient" 1) newBlockchain
      proof = calculateProofOfWork $ calculateHash $ last (blocks blockchain)
  return $ execState (addNewBlock proof Nothing currentTime) blockchain


main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< makeApplication


makeApplication :: IO Application
makeApplication = do
  bc <- myBlockchain
  bws <- newBlockchainWebServiceHandle bc
  return $ serve httpApi (server bws)


httpApi :: Proxy HttpApi
httpApi = Proxy


type HttpApi = "healthcheck" :> Get '[JSON] HealthCheck
          -- new transaction
          :<|> "transactions" :> "new" :> ReqBody '[JSON] Transaction :>  Post '[JSON] NoContent
          -- mines a new block
          :<|> "mine" :> Post '[JSON] NoContent
          -- returns whole blochchain
          :<|> "chain" :> Get '[JSON] Blockchain
          -- accepts a list of new nodes
          :<|> "nodes" :> "register" :> ReqBody '[JSON] [Node] :> Post '[JSON] NoContent
          -- checks and sets the correct chain in the current node
          :<|> "nodes" :> "resolve" :> Post '[JSON] NoContent


server :: BlockchainWebService -> Server HttpApi
server bws = toApi getHealthCheck
        :<|> toApi newTransaction
        :<|> toApi mineBlock
        :<|> toApi getBlockchain
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

