module Blockchain.Node.Service.ServerSpec where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Time (getCurrentTime)
import           Test.Hspec

import           Blockchain.Node.Config (BlockchainConfig(..), defaultConfig)
import           Blockchain.Node.Core (NodeState(..), Block(..), Node(..), runApp, getLength,
                 mineNewBlock, newNodeState, newTransaction, runApp, isValidBlock,
                 getConfirmedTransactions)
import qualified Blockchain.Node.MemPool as MemPool
import qualified Blockchain.Node.NodesNetwork as NodesNetwork
import qualified Blockchain.Node.Transaction as T
import qualified Blockchain.Node.Service as Service
import qualified Blockchain.Node.Service.Server as BlockchainService


testConfig :: BlockchainConfig
testConfig = defaultConfig { miningDifficulty = 2 }


newBlockchainService :: IO (Text, Service.BlockchainService IO)
newBlockchainService = do
  nodeState <- newNodeState testConfig
  nodesNetworkService <- NodesNetwork.newHandle nodeState
  service <- BlockchainService.newHandle nodeState nodesNetworkService
  return (uuid nodeState, service)


spec :: Spec
spec = do
  describe "Transactions" $ do
    it "adds a transaction, returned to-be-mined block index is equal to the blockchain length" $ do
      currentTime <- getCurrentTime
      nodeState <- newNodeState testConfig
      newBlockIndex <- runApp
              nodeState
              (newTransaction "recipient" 1 currentTime)
      blockchainLength <- runApp nodeState getLength
      newBlockIndex `shouldBe` blockchainLength

  describe "Blocks" $ do
    it "mines new block" $ do
      currentTime <- getCurrentTime
      nodeState <- newNodeState testConfig
      newBlock <- runApp nodeState (mineNewBlock currentTime)
      case newBlock of
        Nothing -> False `shouldBe` True
        Just block -> isValidBlock block (miningDifficulty testConfig) `shouldBe` True

    it "adds transaction to the blockchain, mines new block block, and \
    \verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      nodeState <- newNodeState testConfig
      newBlock <- runApp nodeState $ do
                  -- will be rejected
                  _ <- mineNewBlock currentTime
                  newTransaction "recipient" 1 currentTime
                  mineNewBlock currentTime
      putStrLn $ show newBlock
      (length <$> transactions <$> newBlock) `shouldBe` Just 2

    it "adds transaction from and to source addresss to the blockchain, mines \
    \new block block, and verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      nodeState <- newNodeState testConfig
      -- blockchain with 1 current transaction
      _ <- runApp nodeState $
          newTransaction (uuid nodeState) 1 currentTime
      currentMemPool <- readTVarIO $ memPool nodeState
      let currentTransactions = MemPool.unMemPool currentMemPool
      length currentTransactions `shouldBe` 1

      let recentTransaction = currentTransactions !! 0
      (fromJust $ T.sender $ T.operation recentTransaction)
        `shouldBe`
        (T.recipient $ T.operation recentTransaction)

      transactionsInBlockchain <- runApp nodeState getConfirmedTransactions
      transactionsInBlockchain `shouldBe` []

      newBlock <- runApp nodeState $ mineNewBlock currentTime
      (length <$> transactions <$> newBlock) `shouldBe` Just 2

  describe "Service - Register Nodes" $ do
    it "adds an invalid node - invalid URL" $ do
      (_, service) <- newBlockchainService
      _ <- Service.registerNodes service [Node "samplename" "invalid url"]
      nodesFromService <- Service.getNodes service
      -- includes this node
      length nodesFromService `shouldBe` 1
    it "adds an invalid node - this node" $ do
      (blockchainUuid, service) <- newBlockchainService
      _ <- Service.registerNodes service [Node blockchainUuid "http://localhost:8000"]
      nodesFromService <- Service.getNodes service
      -- includes this node
      length nodesFromService `shouldBe` 1
    it "adds a valid node" $ do
      (_, service) <- newBlockchainService
      _ <- Service.registerNodes service [Node "samplename" "http://127.0.1.10:8000"]
      nodesFromService <- Service.getNodes service
      -- includes this node
      length nodesFromService `shouldBe` 2
