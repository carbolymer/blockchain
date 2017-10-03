import Control.Monad.State (State, evalState, execState, runState)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Test.Hspec
import Test.QuickCheck

import Blockchain (Blockchain(..), Block(..), addNewBlock, calculateHash, calculateProofOfWork, getLength, isValidProof, newBlockchain,
    newTransaction)


runBlockchain :: Blockchain -> State Blockchain a -> (a, Blockchain)
runBlockchain = flip runState

evalBlockchain :: Blockchain -> State Blockchain a -> a
evalBlockchain = flip evalState



main :: IO ()
main = hspec $ do
  describe "Transactions" $ do
    it "adds a transaction, returned to-be-mined block index is equal to the blockchain length" $ do
      let (newBlockIndex, blockchain) = runBlockchain newBlockchain (newTransaction "sender" "recipient" 1)
          blockchainLength = evalBlockchain blockchain getLength
      newBlockIndex `shouldBe` blockchainLength

  describe "Blocks" $ do
    it "adds transaction to the blockchain, adds block, and verifies that block contains this transaction" $ do
      currentTime <- getCurrentTime
      -- blockchain with 1 current transaction
      let blockchain = execState (newTransaction "sender" "recipient" 1) newBlockchain
          lastCurrentTransaction = last $ currentTransactions blockchain
          proof = calculateProofOfWork $ calculateHash $ last (blocks blockchain)
          maybeNewBlock = evalBlockchain blockchain (addNewBlock proof Nothing currentTime )
      maybeNewBlock `shouldSatisfy` (not . (== Nothing))
      let newBlock = fromJust maybeNewBlock
      (length $ transactions newBlock) `shouldBe` 1
      lastCurrentTransaction `shouldBe` (last $ transactions newBlock)
