{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.RestApi.Client
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Defines REST API for the node
--
-----------------------------------------------------------------------------
module Blockchain.Node.RestApi.Client (
    NodeRequest (..)
  , RequestException
  , newBlockchainRestApiClient
  , runRequests
) where


import Control.Monad.Catch (Exception, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Concurrent.Async (forConcurrently)
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (ClientEnv(..), ClientM, ServantError, client, runClientM)
import Servant.Common.BaseUrl (BaseUrl, InvalidBaseUrlException, parseBaseUrl)

import Blockchain.Node.Core (Node(..))
import Blockchain.Node.Service (BlockchainService(..))
import Blockchain.Node.RestApi (restApi)


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
runRequests :: (MonadIO m) => [NodeRequest a] -> m [Either RequestException a]
runRequests requests = do
  -- TODO store manager in the app state maybe?
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ forConcurrently requests $ \request -> runExceptT $ do
    nodeUrl <- getNodeUrl request
    runClientExT (nodeRequest request) (ClientEnv manager nodeUrl)
  where
    getNodeUrl :: NodeRequest a -> ExceptT RequestException IO BaseUrl
    getNodeUrl request = (toRequestException :: ExceptionMapperT InvalidBaseUrlException BaseUrl) $
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
