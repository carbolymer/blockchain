import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Test.Hspec

import BlockchainConfig (BlockchainConfig(..), defaultConfig)
import Blockchain (Blockchain(..), Block(..), addNewBlock, blocks, calculateHash, calculateProofOfWork, evalApp,
      execApp, getLength, newBlockchain, newTransaction, runApp, sha256Hash)



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
      let (newBlockIndex, blockchain) = runApp defaultConfig (newTransaction "sender" "recipient" 1) createdBlockchain
      let blockchainLength = evalApp defaultConfig getLength blockchain
      newBlockIndex `shouldBe` blockchainLength

  describe "Blocks" $ do
    it "adds transaction to the blockchain, adds block, and verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      -- blockchain with 1 current transaction
      currentBlockchain <- newBlockchain
      let blockchain = execApp defaultConfig (newTransaction "sender" "recipient" 1) currentBlockchain
      let lastCurrentTransaction = last $ currentTransactions blockchain
          proof = calculateProofOfWork (calculateHash $ last (blocks blockchain)) (miningDifficulty defaultConfig)
          maybeNewBlock = evalApp defaultConfig (addNewBlock proof Nothing currentTime) blockchain
      maybeNewBlock `shouldSatisfy` (not . (== Nothing))
      let newBlock = fromJust maybeNewBlock
      (length $ transactions newBlock) `shouldBe` 1
      lastCurrentTransaction `shouldBe` (last $ transactions newBlock)
