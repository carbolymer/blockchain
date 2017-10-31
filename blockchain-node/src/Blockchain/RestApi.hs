{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
module Blockchain.RestApi (
    RestApi
  , restApi
) where

import Servant ((:>), (:<|>)(..), Get, JSON, Post, Proxy(..), ReqBody)

import Blockchain.Core (Block, Node, Transaction,)
import Blockchain.Service (HealthCheck(..), StatusMessage)


-- | The definition of the node API
type RestApi = "healthcheck" :> Get '[JSON] HealthCheck
          -- new transaction
          :<|> "transactions" :> "new" :> ReqBody '[JSON] Transaction :>  Post '[JSON] StatusMessage
          -- list of confirmed transactions in the chain
          :<|> "transactions" :> "confirmed" :> Get '[JSON] [Transaction]
          -- list of not confirmed transactions
          :<|> "transactions" :> "unconfirmed" :>  Get '[JSON] [Transaction]
          -- mines a new block
          :<|> "mine" :> Post '[JSON] (Maybe Block)
          -- returns whole blochchain
          :<|> "chain" :> Get '[JSON] [Block]
          -- returns a list of nodes known to this one
          :<|> "nodes" :> "list" :> Get '[JSON] [Node]
          -- accepts a list of new nodes
          :<|> "nodes" :> "register" :> ReqBody '[JSON] [Node] :> Post '[JSON] StatusMessage
          -- checks and sets the correct chain in the current node
          :<|> "nodes" :> "resolve" :> Post '[JSON] StatusMessage


-- | A proxy to the RestApi
restApi :: Proxy RestApi
restApi = Proxy
