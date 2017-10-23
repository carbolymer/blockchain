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
) where

import Servant ((:<|>)(..))
import Servant.Client (client, ClientM)

import Blockchain.Core (Node)
import Blockchain.Service (BlockchainService(..))
import Blockchain.RestApi (RestApi, restApi)


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
