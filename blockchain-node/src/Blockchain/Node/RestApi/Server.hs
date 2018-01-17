--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
module Blockchain.Node.RestApi.Server (
  bootstrap
) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Function((&))
import           Network.HostName (getHostName)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                  setBeforeMainLoop, setPort)
import           Servant ((:<|>)(..), Application, Server, serve)

import           Blockchain.Node.Core (newNodeState, uuid)
import           Blockchain.Node.Config (BlockchainConfig)
import qualified Blockchain.Node.Config as Config
import qualified Blockchain.Node.NodesNetwork as NodesNetwork
import qualified Blockchain.Node.Service as Service
import           Blockchain.Node.Service (BlockchainService)
import qualified Blockchain.Node.Service.Server as BlockchainService
import           Blockchain.Node.RestApi (RestApi, restApi)
import           Logger
import           RestUtil (convert)


infoL :: (MonadIO m) => String -> m ()
[infoL] = getLogger "Blockchain.Node.RestApi.Server" [INFO]


server :: BlockchainService IO -> Server RestApi
server service = toApi Service.getHealthCheck
            :<|> toApi Service.newTransaction
            :<|> toApi Service.getConfirmedTransactions
            :<|> toApi Service.getUnconfirmedTransactions
            :<|> toApi Service.mineBlock
            :<|> toApi Service.getBlockchain
            :<|> toApi Service.getNodes
            :<|> toApi Service.registerNodes
            :<|> toApi Service.resolveNodes
                where
                  toApi method = convert (service & method)


-- | Starts the web application
bootstrap :: BlockchainConfig -> IO ()
bootstrap config = do
  hostname <- getHostName
  infoL "~~~~ Starting Node ~~~~"
  let settings =
        setPort (Config.httpPort config) $
        setBeforeMainLoop (infoL $ "listening on endpoint http://" ++
            hostname ++ ":" ++ show (Config.httpPort config)) $
        defaultSettings
  runSettings settings =<< (makeApplication config)


makeApplication :: BlockchainConfig -> IO Application
makeApplication config = do
  nodeState <- newNodeState config
  nodesNetworkService <- NodesNetwork.newHandle nodeState
  bws <- BlockchainService.newHandle nodeState nodesNetworkService
  infoL $ "This node UUID: " ++ (show $ uuid nodeState)
  return $ serve restApi (server bws)
