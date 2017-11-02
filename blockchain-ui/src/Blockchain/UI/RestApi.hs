{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.UI.RestApi
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Defines REST API for the UI
--
-----------------------------------------------------------------------------
module Blockchain.UI.RestApi (
    UiApi
  , uiApi
) where

import Data.Text (Text)
import Servant ((:>), (:<|>)(..), Capture, Get, JSON, Post, Proxy(..), ReqBody, Raw)

import Blockchain.UI.Core (NodeDto, Account)
import Blockchain.Node.Core (Transaction)
import Blockchain.Node.Service (StatusMessage)


-- | The definition of the node API + static files
type UiApi = "api" :> "nodes" :> Get '[JSON] [NodeDto]
        :<|> "api" :> "nodes" :> "mine" :> Capture "nodeId" Text :> Post '[JSON] StatusMessage
        :<|> "api" :> "transactions" :> Get '[JSON] [Transaction]
        :<|> "api" :> "transactions" :> "new" :> ReqBody '[JSON] Transaction :> Post '[JSON] StatusMessage
        :<|> "api" :> "accounts" :> Get '[JSON] [Account]
        :<|> Raw

uiApi :: Proxy UiApi
uiApi = Proxy
