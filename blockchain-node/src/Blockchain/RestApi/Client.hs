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

import Control.Arrow (left)
import Control.Monad (forM)
import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (ClientEnv(..), ClientM, ServantError, client, runClientM)
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

newtype RequestException = RequestException String deriving (Show, Typeable)
instance Exception RequestException

toRequestException :: (Exception e) => e -> RequestException
toRequestException exception = RequestException $ show exception 

-- TODO refactor this mess
runRequests :: [NodeRequest a] -> BlockchainApp ([Either RequestException a])
runRequests requests = do
  -- TODO store manager in the app state maybe?
  manager <- liftIO $ newManager defaultManagerSettings
--   nodes <- liftIO =<< readTVarIO <$> nodes <$> get
  forM requests $ \request -> liftIO $ runExceptT $ do
    nodeUrl <- getNodeUrl request
    let env = ClientEnv manager nodeUrl
    runClientExT (nodeRequest request) env
    where
      getNodeUrl :: NodeRequest a -> ExceptT RequestException IO BaseUrl
      getNodeUrl request = do
        convertInvalidBaseUrlException $ ExceptT $ try $ parseBaseUrl $ unpack $ url $ targetNode request

      runClientExT :: ClientM a -> ClientEnv -> ExceptT RequestException IO a
      runClientExT request env = convertServantError $ ExceptT $ runClientM request env
        
      convertServantError :: ExceptT ServantError IO a -> ExceptT RequestException IO a
      convertServantError = withExceptT toRequestException

      convertInvalidBaseUrlException :: ExceptT InvalidBaseUrlException IO a -> ExceptT RequestException IO a
      convertInvalidBaseUrlException = withExceptT toRequestException
