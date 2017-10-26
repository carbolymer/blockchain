{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.RestApi
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Defines REST API for the node
--
-----------------------------------------------------------------------------
module Blockchain.RestApi.Client (
    newBlockchainRestApiClient
  , runRequests
) where

import Control.Monad (forM)
import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (ClientEnv(..), ClientM, client, runClientM)
import Servant.Common.BaseUrl (BaseUrl, InvalidBaseUrlException, parseBaseUrl)

import Blockchain.Core (Blockchain(..), BlockchainApp(..), Node(..))
import Blockchain.Service (BlockchainService(..))
import Blockchain.RestApi (RestApi, restApi)
import Logger


-- [infoL, errorL] = getLogger "Blockchain.RestApi.Client" [INFO, ERROR]


-- | Creates new Blockchain REST api client
newBlockchainRestApiClient :: BlockchainService ClientM
newBlockchainRestApiClient = do
  let getHealthCheck
        :<|> newTransaction
        :<|> getConfirmedTransactions
        :<|> getUnconfirmedTransactions
        :<|> mineBlock
        :<|> getBlockchain
        :<|> getNodes
        :<|> registerNodes
        :<|> resolveNodes = client restApi
  BlockchainService {..}


data NodeRequest a = NodeRequest {
  targetNode :: Node,
  nodeRequest :: ClientM a
}

newtype RunRequestException = RunRequestException String deriving (Show, Typeable)
instance Exception RunRequestException

-- TODO refactor this mess
runRequests :: [NodeRequest a] -> BlockchainApp ([Either RunRequestException a])
runRequests requests = do
  -- TODO store manager in the app state maybe?
  manager <- liftIO $ newManager defaultManagerSettings
--   nodes <- liftIO =<< readTVarIO <$> nodes <$> get
  forM requests $ \request -> liftIO $ try $ do
    targetNodeUrl <- getNodeUrl request
    let env = ClientEnv manager targetNodeUrl
    result <- runClientM (nodeRequest request) env
    case result of
      Left error ->
          throwM $ RunRequestException $ "Error during calling node: " ++ (show $ targetNode request) ++ " " ++ (show error)
      Right response -> return response
    where
      getNodeUrl :: NodeRequest a -> IO BaseUrl
      getNodeUrl request = do
        nodeUrl_ <- getNodeUrl_ request
        case nodeUrl_ of
          Left error -> throwM $ RunRequestException $ show error
          Right baseUrl -> return baseUrl

      getNodeUrl_ :: NodeRequest a -> IO (Either InvalidBaseUrlException BaseUrl)
      getNodeUrl_ request = try $ parseBaseUrl $ unpack $ url $ targetNode request
