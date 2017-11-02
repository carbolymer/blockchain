import           Data.Time (getCurrentTime)
import           Test.Hspec

import           Blockchain.Node.Config (BlockchainConfig(..), defaultConfig)
import           Blockchain.Node.Core (Blockchain(..), Block(..), Node(..), evalApp, getLength, mineNewBlock, newBlockchain, newTransaction, runApp, sha256Hash)
import qualified Blockchain.Node.Service as Service
import           Blockchain.Node.Service.Server (newBlockchainServiceHandle)

testConfig :: BlockchainConfig
testConfig = defaultConfig { miningDifficulty = 2 }

main :: IO ()
main = hspec $ do
  describe "Test hashing" $ do
    it "hashes test string, compares with precalculated hash" $ do
      let testString = "testhash"
          testHash = "4bc75035d73f6083683e040fc31f28e0ec6d1cbce5cb0a5e2611eb89bceb6c16"
      sha256Hash testString `shouldBe` testHash


  describe "Transactions" $ do
    it "adds a transaction, returned to-be-mined block index is equal to the blockchain length" $ do
      createdBlockchain <- newBlockchain
      (newBlockIndex, blockchain) <- runApp testConfig (newTransaction "sender" "recipient" 1) createdBlockchain
      blockchainLength <- evalApp testConfig getLength blockchain
      newBlockIndex `shouldBe` blockchainLength

  describe "Blocks" $ do
    it "adds transaction to the blockchain, mines new block block, and verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      -- blockchain with 1 current transaction
      currentBlockchain <- newBlockchain
      let createNewTransactionAndNewBlock currentTime = do
                    _ <- newTransaction "sender" "recipient" 1 currentTime
                    mineNewBlock currentTime
      (newBlock, _) <- runApp testConfig (createNewTransactionAndNewBlock currentTime) currentBlockchain
      (length <$> transactions <$> newBlock) `shouldBe` Just 2

  describe "Service - Register Nodes" $ do
    it "adds an invalid node - invalid URL" $ do
      blockchain <- newBlockchain
      service <- newBlockchainServiceHandle testConfig blockchain
      _ <- Service.registerNodes service [Node "samplename" "invalid url"]
      nodesFromService <- Service.getNodes service
      length nodesFromService `shouldBe` 0
    it "adds an invalid node - this node" $ do
      blockchain <- newBlockchain
      service <- newBlockchainServiceHandle testConfig blockchain
      _ <- Service.registerNodes service [Node (uuid blockchain) "http://localhost:8000"]
      nodesFromService <- Service.getNodes service
      length nodesFromService `shouldBe` 0
    it "adds a valid node" $ do
      blockchain <- newBlockchain
      service <- newBlockchainServiceHandle testConfig blockchain
      _ <- Service.registerNodes service [Node "samplename" "http://127.0.1.10:8000"]
      nodesFromService <- Service.getNodes service
      length nodesFromService `shouldBe` 1
