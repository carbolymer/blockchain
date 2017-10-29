{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
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
    NodeRequest (..)
  , RequestException
  , newBlockchainRestApiClient
  , runRequests
) where


import Control.Monad (forM)
import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Concurrent.Async (forConcurrently)
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

-- | Represents node and its request pair
data NodeRequest a = NodeRequest {
  targetNode :: Node,       -- ^ request target
  nodeRequest :: ClientM a  -- ^ a request
}


-- | Processes list of node requests and returns a list of results or request exceptions
runRequests :: [NodeRequest a] -> BlockchainApp ([Either RequestException a])
runRequests requests = do
  -- TODO store manager in the app state maybe?
  manager <- liftIO $ newManager defaultManagerSettings
--   nodes <- liftIO =<< readTVarIO <$> nodes <$> get
  liftIO $ forConcurrently requests $ \request -> runExceptT $ do
    nodeUrl <- getNodeUrl request
    runClientExT (nodeRequest request) (ClientEnv manager nodeUrl)
  where
    getNodeUrl :: NodeRequest a -> ExceptT RequestException IO BaseUrl
    getNodeUrl request = (toRequestException :: ExceptionMapperT InvalidBaseUrlException a) $
        try $ parseBaseUrl $ unpack $ url $ targetNode request

    runClientExT :: ClientM a -> ClientEnv -> ExceptT RequestException IO a
    runClientExT request env = (toRequestException :: ExceptionMapperT ServantError a) $
        runClientM request env


--
-- Exception handlers
--

-- | Request exception - when something occured during request execution
newtype RequestException = RequestException String deriving (Show, Typeable)
instance Exception RequestException


-- | Mapper maps exception `e` to RequestException
type ExceptionMapperT e a = (Exception e) => IO (Either e a) -> ExceptT RequestException IO a


-- | Generic exception mapper for Excepions
toRequestException :: ExceptionMapperT e a
toRequestException ioEither = withExceptT (RequestException . show) (ExceptT ioEither)
