{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Blockchain.UI.Core (
    AppState(..)
  , Account(..)
  , NodeDto(..)
  , getAccounts
  , mineOnNode
  , newApp
  , runBackgroundStateUpdater
  , createNewTransaction
) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever, forM, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Either (lefts, rights)
import           Data.Foldable (foldr')
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import           Data.Text (Text, pack, unpack)
import           GHC.Generics (Generic)
import           Servant.Client (ClientM)

import qualified Blockchain.Node.Config as N
import           Blockchain.Node.Core ((<$$>))
import qualified Blockchain.Node.Core (Node(..), Transaction(..), isValidChain)
import           Blockchain.Node.Service (BlockchainService(..), MessageLevel(..), StatusMessage(..))
import           Blockchain.Node.RestApi.Client (NodeRequest(..), newBlockchainRestApiClient, runRequests)
import           Blockchain.UI.Config (UiConfig(..))
import qualified Logger


infoL, warnL, traceL :: (MonadIO m) => String -> m ()
[infoL, warnL, traceL] = Logger.getLogger "Blockchain.UI.Core" [Logger.INFO, Logger.WARNING, Logger.TRACE]

data AppState = AppState {
  config :: !UiConfig,
  nodes :: IORef [NodeDto],
  transactions :: IORef [Blockchain.Node.Core.Transaction],
  restClient :: BlockchainService ClientM
}


newApp :: UiConfig -> IO AppState
newApp config = do
  emptyNodes <- newIORef []
  emptyTransactions <- newIORef []
  return $ AppState config emptyNodes emptyTransactions newBlockchainRestApiClient


data NodeDto = NodeDto {
  id :: !Text,
  url :: !Text,
  validChainLength :: !Int
} deriving (Eq, Show, Generic)

instance ToJSON NodeDto
instance FromJSON NodeDto


data Account = Account {
  accountId :: !Text,
  balance :: !Int,
  accountTransactions :: ![Blockchain.Node.Core.Transaction]
} deriving (Eq, Show, Generic)

instance ToJSON Account
instance FromJSON Account


nodeDtoToNode :: NodeDto -> Blockchain.Node.Core.Node
nodeDtoToNode dto = Blockchain.Node.Core.Node (Blockchain.UI.Core.id dto) (url dto)


nodeToNodeDto :: Blockchain.Node.Core.Node -> NodeDto
nodeToNodeDto node = NodeDto (Blockchain.Node.Core.id node) (Blockchain.Node.Core.url node) 0


runBackgroundStateUpdater :: AppState -> IO ()
runBackgroundStateUpdater app = void $ forkIO $ forever $ do
  nodesList <- (map nodeDtoToNode) <$> (readIORef $ nodes app)

  newNodesList <- getNodesFromList app (getNonEmptyNodesList nodesList)
  writeIORef (nodes app) newNodesList
--   traceL $ "Current nodes list: " ++ (show newNodesList)

  let nonEmptyNodesList = getNonEmptyNodesList $ map nodeDtoToNode newNodesList
  fetchedTransactions <- getTransactionsFromNodes app nonEmptyNodesList
  writeIORef (transactions app) fetchedTransactions
--   traceL $ "Current transactions list: " ++ (show fetchedTransactions)

  threadDelay $ nodesListRefreshInterval $ config app
  where
    getNonEmptyNodesList nodes = case length nodes of
      0 -> [Blockchain.Node.Core.Node "" (beaconNodeUrl $ config app)]
      _ -> nodes


getNodesFromList :: AppState -> [Blockchain.Node.Core.Node] -> IO [NodeDto]
getNodesFromList app nodes = do
--   infoL "Fetching nodes list from all nodes"
  let requests = map (\node -> NodeRequest node (getNodes $ restClient app)) nodes
  result <- runRequests requests
  mapM_ (warnL . show) (lefts result)
  -- unique list
  let nodesList = S.toList $ S.fromList $ concat $ rights result
  catMaybes <$> forM nodesList (queryNodeAndCreateDto app)



queryNodeAndCreateDto :: AppState -> Blockchain.Node.Core.Node -> IO (Maybe NodeDto)
queryNodeAndCreateDto app node = do
  let dto = nodeToNodeDto node
  let requests = [NodeRequest node (getBlockchain $ restClient app)]
  result <- runRequests requests
  mapM_ (warnL . show) (lefts result)
  case rights result of
   [] -> return Nothing
   [chain] -> case Blockchain.Node.Core.isValidChain (N.miningDifficulty N.defaultConfig) chain of
     True -> do
--         traceL $ "Chain length for: " ++ (show $ Blockchain.Node.Core.id node) ++ ": " ++ (show $ length chain)
        return $ Just $ dto {validChainLength = length chain}
     False -> return $ Just dto
   _ -> return Nothing -- impossible case


getTransactionsFromNodes :: AppState -> [Blockchain.Node.Core.Node] -> IO [Blockchain.Node.Core.Transaction]
getTransactionsFromNodes app nodes = do
  --   infoL "Fetching nodes list from all nodes"
  let requests = map (\node -> NodeRequest node (getConfirmedTransactions $ restClient app)) nodes
  result <- runRequests requests
  mapM_ (warnL . show) (lefts result)
  -- unique list
  return $ S.toList $ S.fromList $ concat $ rights result


mineOnNode :: AppState -> Text -> IO StatusMessage
mineOnNode app nodeId = do
  let filterNodeById = filter ((== nodeId) . Blockchain.UI.Core.id)
  maybeTargetNode <- listToMaybe <$> filterNodeById <$> (readIORef $ nodes app)
  case maybeTargetNode of
    Nothing -> return $ StatusMessage ERROR "Node does not exist"
    Just targetNode -> do
      let node = nodeDtoToNode targetNode
      result <- runRequests $ [NodeRequest node (mineBlock $ restClient app)]
      mapM_ (warnL . show) (lefts result)
      return $ case rights result of
        [] -> StatusMessage ERROR "No block was mined"
        [Nothing] -> StatusMessage ERROR "New block was mined, but could not be attached to the blockchain"
        [Just _] -> StatusMessage INFO "Block was mined successfully"
        impossibleCase -> StatusMessage ERROR $ pack $ "Impossible case: " ++ (show impossibleCase)



(<+|) :: Account -> Blockchain.Node.Core.Transaction -> Account
(<+|) account transaction
  | isOutgoing && isIncoming  = account {
                                  accountTransactions = accountTransactions account ++ [transaction]
                                }
  | isOutgoing                = account {
                                  balance             = balance account - Blockchain.Node.Core.amount transaction,
                                  accountTransactions = accountTransactions account ++ [transaction]
                                }
  | isIncoming                = account {
                                  balance             = balance account + Blockchain.Node.Core.amount transaction,
                                  accountTransactions = accountTransactions account ++ [transaction]
                                }
  | otherwise                 = account
    where
      isOutgoing = accountId account == Blockchain.Node.Core.sender transaction
      isIncoming = accountId account == Blockchain.Node.Core.recipient transaction


getAccounts :: AppState -> IO [Account]
getAccounts app = do
  transactionsList <- readIORef $ transactions app
  return $ Map.elems $ foldr' insertIntoAccountMap Map.empty transactionsList


insertIntoAccountMap :: Blockchain.Node.Core.Transaction -> Map.Map Text Account -> Map.Map Text Account
insertIntoAccountMap transaction accountMap = do
  let getOrCreate accId = fromMaybe (Account accId 0 []) (Map.lookup accId accountMap)
      recipientId = Blockchain.Node.Core.recipient transaction
      senderId = Blockchain.Node.Core.sender transaction
      recipientAccount = getOrCreate recipientId
      senderAccount = getOrCreate senderId

  Map.insert recipientId (recipientAccount <+| transaction) $
    Map.insert senderId (senderAccount <+| transaction) accountMap


createNewTransaction :: AppState -> Blockchain.Node.Core.Transaction -> IO StatusMessage
createNewTransaction app transaction = do
  targetNode <- nodeDtoToNode <$$> listToMaybe <$> filter ((== txSender) . Blockchain.UI.Core.id) <$> (readIORef $ nodes app)
  case targetNode of
    Nothing -> return $ StatusMessage ERROR $ pack $ "No such node: " ++ (unpack txSender)
    Just node -> do
      result <- runRequests [NodeRequest node (newTransaction (restClient app) transaction)]
      mapM_ (warnL . show) (lefts result)
      return $ case rights result of
        [] -> StatusMessage ERROR "Could not send transaction"
        [response] -> response
        impossibleCase -> StatusMessage ERROR "Impossible happened!"

  where
    txAmount = Blockchain.Node.Core.amount transaction
    txSender = Blockchain.Node.Core.sender transaction
