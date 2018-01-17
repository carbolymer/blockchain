module Blockchain.Node.AccountSpec where

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time (getCurrentTime)
import           Test.Hspec

import           Blockchain.Node.Account ((<+|), Account)
import qualified Blockchain.Node.Account as Acc
import           Blockchain.Node.Config (BlockchainConfig(..), defaultConfig)
import qualified Blockchain.Node.Core as Core
import qualified Blockchain.Node.MemPool as MemPool
import           Blockchain.Node.Transaction (Transaction, Operation(..))
import qualified Blockchain.Node.Transaction as T
import qualified Blockchain.Node.Signature as Signature


testConfig :: BlockchainConfig
testConfig = defaultConfig { miningDifficulty = 1 }


newTransfer :: Signature.KeyPair -> Text -> Text -> Int -> IO Transaction
newTransfer (pubKey, privKey) sender recipient amount = do
  currentTime <- getCurrentTime
  let operation = Transfer sender recipient amount currentTime
  signature <- T.sign privKey operation
  return $ T.Transaction
    pubKey
    signature
    operation


spec :: Spec
spec = do
  describe "Transaction validation" $ do
    it "creates new mining reward, new transaction and validates mempool" $ do
      currentTime <- getCurrentTime
      nodeState <- Core.newNodeState testConfig
      _ <- Core.runApp nodeState $ do
        reward <- Core.getMiningReward currentTime
        _ <- Core.addTransaction reward -- add to mempool
        Core.newTransaction "randomguy123" 1 currentTime
      memPool <- readTVarIO $ Core.memPool nodeState
      let memPoolTransactions = MemPool.unMemPool memPool
      (length memPoolTransactions) `shouldBe` 2

  describe "Addition operator <+| test" $ do
    it "creates empty account for sender and recipient, and inserts transaction" $ do
      let senderId = "sender"
          recipientId = "recipient"
          senderAccount = Acc.getOrCreate senderId Map.empty
          recipientAccount = Acc.getOrCreate recipientId Map.empty
      keyPair <- Signature.newKeyPair
      transaction <- newTransfer keyPair senderId recipientId 1
      let accountMap = Acc.toAccountsByAddress [transaction]

      Acc.isNotGeneratingNegativeBalance transaction Map.empty `shouldBe` False
      -- recipient has positive balance
      Acc.isValid (recipientAccount <+| transaction) `shouldBe` True
      Acc.balance (recipientAccount <+| transaction) `shouldBe` 1
      -- sender has negative balance
      Acc.isValid (senderAccount <+| transaction) `shouldBe` False
      Acc.balance (senderAccount <+| transaction) `shouldBe` -1


    it "creates empty account for sender and recipient (which are the same), \
    \and inserts transaction" $ do
      let senderId = "sender"
          recipientId = senderId
          senderAccount = Acc.getOrCreate senderId Map.empty
          recipientAccount = Acc.getOrCreate recipientId Map.empty
      keyPair <- Signature.newKeyPair
      transaction <- newTransfer keyPair senderId recipientId 1
      let accountMap = Acc.toAccountsByAddress [transaction]

      Acc.isNotGeneratingNegativeBalance transaction Map.empty `shouldBe` True
      -- recipient has positive balance
      Acc.isValid (recipientAccount <+| transaction) `shouldBe` True
      Acc.balance (recipientAccount <+| transaction) `shouldBe` 0
      -- sender has negative balance
      Acc.isValid (senderAccount <+| transaction) `shouldBe` True
      Acc.balance (senderAccount <+| transaction) `shouldBe` 0
