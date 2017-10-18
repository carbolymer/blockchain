import Data.Time (getCurrentTime)
import Test.Hspec

import BlockchainConfig (BlockchainConfig(..), defaultConfig)
import Blockchain (Block(..), evalApp, getLength, mineNewBlock, newBlockchain, newTransaction, runApp,
    sha256Hash)

testConfig :: BlockchainConfig
testConfig = defaultConfig { miningDifficulty = 1 }


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
      let (newBlockIndex, blockchain) = runApp testConfig (newTransaction "sender" "recipient" 1) createdBlockchain
          blockchainLength = evalApp testConfig getLength blockchain
      newBlockIndex `shouldBe` blockchainLength

  describe "Blocks" $ do
    it "adds transaction to the blockchain, mines new block block, and verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      -- blockchain with 1 current transaction
      currentBlockchain <- newBlockchain
      let (newBlock, blockchain) = runApp testConfig (createNewTransactionAndNewBlock currentTime) currentBlockchain
      (length $ transactions newBlock) `shouldBe` 2
      where
        createNewTransactionAndNewBlock currentTime = do
          newTransaction "sender" "recipient" 1
          mineNewBlock currentTime
