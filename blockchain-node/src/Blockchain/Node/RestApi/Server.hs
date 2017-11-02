
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.RestApi.Server
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Defines REST API for the node. The server part
--
-----------------------------------------------------------------------------
module Blockchain.Node.RestApi.Server (
  bootstrap
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function((&))
import Network.HostName (getHostName)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:<|>)(..), Application, Server, serve)

import Blockchain.Node.Core (newBlockchain)
import Blockchain.Node.Config (BlockchainConfig(..))
import Blockchain.Node.Service (BlockchainService(..))
import Blockchain.Node.Service.Server (newBlockchainServiceHandle)
import Blockchain.Node.RestApi (RestApi, restApi)
import Logger
import RestUtil (convert)

infoL :: (MonadIO m) => String -> m ()
[infoL] = getLogger "Blockchain.Node.RestApi.Server" [INFO]


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
  hostname <- getHostName
  infoL "~~~~ Starting Node ~~~~"
  let settings =
        setPort (httpPort config) $
        setBeforeMainLoop (infoL $ "listening on endpoint http://" ++ hostname ++ ":" ++ show (httpPort config)) $
        defaultSettings
  runSettings settings =<< (makeApplication config)


makeApplication :: BlockchainConfig -> IO Application
makeApplication config = do
  bws <- blockchainService
  return $ serve restApi (server bws)
  where
    blockchainService :: IO (BlockchainService IO)
    blockchainService = newBlockchain >>= (newBlockchainServiceHandle config)
