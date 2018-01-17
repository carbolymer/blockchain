module Blockchain.UI.Service.Server (
    UiService(..)
  , newUiServiceHandle
) where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (readIORef)
import Data.Text (Text)

import Blockchain.Node.Account (Account(accountId))
import Blockchain.Node.Service (StatusMessage)
import Blockchain.Node.Transaction (NewTransactionRequest, Transaction)
import Blockchain.UI.Config (UiConfig)
import Blockchain.UI.Core (AppState(..), NodeDto,
    createNewTransaction, getAccounts, mineOnNode, newApp,
    runBackgroundStateUpdater)


data (MonadIO m) => UiService m = UiService {
  getNodes :: m [NodeDto],
  mine :: Text -> m StatusMessage,
  getTransactions :: m [Transaction],
  newTransaction :: NewTransactionRequest -> m StatusMessage,
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
