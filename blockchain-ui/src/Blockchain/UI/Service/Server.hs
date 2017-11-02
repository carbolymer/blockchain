{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Blockchain.UI.Service.Server (
    UiService(..)
  , newUiServiceHandle
) where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (readIORef)
import Data.Text (Text)

import Blockchain.Node.Core (Transaction)
import Blockchain.Node.Service (StatusMessage)
import Blockchain.UI.Config (UiConfig)
import Blockchain.UI.Core (Account(..), AppState(..), NodeDto, createNewTransaction, getAccounts, mineOnNode, newApp,
  runBackgroundStateUpdater)


data (MonadIO m) => UiService m = UiService {
  getNodes :: m [NodeDto],
  mine :: Text -> m StatusMessage,
  getTransactions :: m [Transaction],
  newTransaction :: Transaction -> m StatusMessage,
  getAccountsList :: m [Account]
}

newUiServiceHandle :: UiConfig -> IO (UiService IO)
newUiServiceHandle config = do
  uiApp <- newApp config
  runBackgroundStateUpdater uiApp
  return UiService {
    getNodes = readIORef $ nodes uiApp,
    mine = mineOnNode uiApp,
    getTransactions = readIORef $ transactions uiApp,
    newTransaction = \transaction -> createNewTransaction uiApp transaction,
    getAccountsList = filter ((/= "0") . accountId) <$> getAccounts uiApp
  }
