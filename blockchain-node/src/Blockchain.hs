{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The blockchain datamodel + core functions
--
-----------------------------------------------------------------------------

module  Blockchain (
  -- * Blockchain Application execution functions
    BlockchainApp
  , execApp
  , evalApp
  , runApp

  -- * Blockchain data model
  -- ** Model
  , Blockchain(..)
  , Block(..)
  , Transaction(..)
  -- ** Constructor functions
  , newBlockchain
  , addNewBlock
  , mineNewBlock
  , addTransaction
  , newTransaction

  -- * Utility functions
  , getLength
  , calculateProofOfWork
  , isValidProof
  , calculateHash
  , sha256Hash
) where


import           Control.Monad.State (get, put, modify)
import           Control.Monad.RWS (RWS, evalRWS, execRWS, runRWS)
import           Control.Monad.Reader (ask)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Char8 (pack)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, replace)
import qualified Data.Text.Encoding as E
import           Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)
import           Data.Time.Calendar (fromGregorian)
import qualified Data.UUID as U
import           GHC.Generics (Generic)
import           System.Random (randomIO)

import           BlockchainConfig (BlockchainConfig(..))


-- | Blockchain application environment
type BlockchainApp = RWS BlockchainConfig () Blockchain

-- | Evaluates computation over an application, and returns final state
execApp :: BlockchainConfig -- ^ config
        -> BlockchainApp a  -- ^ RWS computation
        -> Blockchain       -- ^ a blockchain state
        -> Blockchain       -- ^ final state
execApp config blockchainApp  blockchain = fst $ execRWS blockchainApp config blockchain

-- | Evaluates computation over an application, and returns final value
evalApp :: BlockchainConfig -- ^ config
        -> BlockchainApp a  -- ^ RWS computation
        -> Blockchain       -- ^ a blockchain state
        -> a                -- ^ final value
evalApp config blockchainApp blockchain = fst $ evalRWS blockchainApp config blockchain

-- | Evaluates computation over an application, and returns final value
runApp :: BlockchainConfig -- ^ config
       -> BlockchainApp a  -- ^ RWS computation
       -> Blockchain       -- ^ a blockchain state
       -> (a, Blockchain)  -- ^ final value and blockchain
runApp config blockchainApp blockchain = (result, newBlockchain)
   where (result, newBlockchain, _) = runRWS blockchainApp config blockchain

-- | Current state of the node.
data Blockchain = Blockchain {
  currentTransactions     :: ![Transaction],  -- ^ Current transactions (to be included in the next block)
  blocks                  :: ![Block],        -- ^ The list of valid blocks
  uuid                    :: !Text
} deriving (Show, Eq, Generic)

instance ToJSON Blockchain
instance FromJSON Blockchain


-- | A building block of the blockchain
data Block = Block {
  index           :: !Int,            -- ^ Position in the blockchain
  previousHash    :: !BS.ByteString,  -- ^ Hash of the previous block
  timestamp       :: !UTCTime,        -- ^ Block creation time
  transactions    :: ![Transaction],  -- ^ List of transactions within the block
  proof           :: !Int             -- ^ Proof of work
} deriving (Show, Eq, Generic)

instance ToJSON Block
instance FromJSON Block

instance ToJSON BS.ByteString where
  toJSON = toJSON . E.decodeUtf8

instance FromJSON BS.ByteString where
  parseJSON (A.String v) = return $ E.encodeUtf8 v
  parseJSON invalid = A.typeMismatch "ByteString" invalid


-- | Represents single transaction between two adresses
data Transaction = Transaction {
  sender      :: !Text,   -- ^ Sender address
  recipient   :: !Text,   -- ^ Recipient address
  amount      :: !Double     -- ^ Transferred amount
} deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction


-- | Creates new blockchain with the genesis block
newBlockchain :: IO Blockchain
newBlockchain = do
  nodeUuid <- U.toText <$> (randomIO :: IO U.UUID)
  return $ Blockchain [] [genesisBlock] (replace "-" "" nodeUuid)


-- | Returns current length of the blockchain
getLength :: BlockchainApp Int
getLength = length <$> blocks <$> get


-- | The first block in the blockchain
genesisBlock :: Block
genesisBlock = Block {
  index = 0,
  previousHash = "1",
  timestamp = UTCTime (fromGregorian 2017 10 1) (secondsToDiffTime 0),
  transactions = [],
  proof = 100
}


-- | Adds new transaction to the transactions list, which will be added to the next block
newTransaction :: Text                  -- ^ Sender address
               -> Text                  -- ^ Recipient address
               -> Double                   -- ^ Transfer amount
               -> BlockchainApp Int     -- ^ Blockchain along with the index of the next-to-be-mined block
newTransaction sender recipient amount = addTransaction $ Transaction sender recipient amount


-- | Adds new transaction to the transactions list, which will be added to the next block
-- returns blockchain along with the index of the next-to-be-mined block
addTransaction :: Transaction -> BlockchainApp Int
addTransaction transaction = do
  blockchain <- get
  let transactions = currentTransactions blockchain
      newTransactions = transactions ++ [transaction]
  put blockchain { currentTransactions = newTransactions}
  getLength


-- | Returns the last block from the blockchain
getLastBlock :: BlockchainApp Block
getLastBlock = last <$> blocks <$> get


{-# DEPRECATED addNewBlock "Use `mineNewBlock` instead" #-}
-- | Adds new block to the blockchain
addNewBlock :: Int                            -- ^ Proof of work, must be valid
            -> Maybe BS.ByteString            -- ^ Optional hash of the previous block, if not provided, will be computed
            -> UTCTime                        -- ^ Creation time
            -> BlockchainApp (Maybe Block)    -- ^ Modified blockchain and new block if the proof is valid, otherwise `Nothing`
addNewBlock newBlockProof previousBlockHash creationTime = do
  lastBlockCalculatedHash <- calculateHash <$> getLastBlock
  difficulty <- miningDifficulty <$> ask
  let lastBlockHash = fromMaybe lastBlockCalculatedHash previousBlockHash

  if isValidProof lastBlockHash newBlockProof difficulty then do
      blockchain <- get
      newBlockIndex <- getLength
      let newBlock = Block {
        index = newBlockIndex,
        previousHash = lastBlockHash,
        timestamp = creationTime,
        transactions = currentTransactions blockchain,
        proof = newBlockProof
      }
      -- update state
      put blockchain {
        currentTransactions = [],
        blocks = (blocks blockchain) ++ [newBlock]
      }
      return $ Just newBlock
    else
      return Nothing


-- | Mines new block in the blockchain
mineNewBlock :: UTCTime             -- ^ Creation time
             -> BlockchainApp Block -- ^ Modified blockchain and the new block
mineNewBlock creationTime = do
  lastBlockHash <- calculateHash <$> getLastBlock
  difficulty <- miningDifficulty <$> ask
  let proofOfWork = calculateProofOfWork lastBlockHash difficulty
  blockchain <- get
  newBlockIndex <- getLength
  rewardTransaction <- getMiningReward creationTime
  let newBlock = Block {
    index = newBlockIndex,
    previousHash = lastBlockHash,
    timestamp = creationTime,
    transactions = (currentTransactions blockchain) ++ [rewardTransaction],
    proof = proofOfWork
  }
  -- update state
  put blockchain {
    currentTransactions = [],
    blocks = (blocks blockchain) ++ [newBlock]
  }
  return newBlock


-- | Returns transaction with the mining reward assigned to the current node
getMiningReward :: UTCTime -> BlockchainApp Transaction
getMiningReward time = do
  reward <- miningReward <$> ask
  recipientAddress <- uuid <$> get
  return $ Transaction "0" recipientAddress reward


-- | Calculates SHA256 hash of the provided argument
sha256Hash :: BS.ByteString -> BS.ByteString
sha256Hash = B16.encode . SHA256.hash


-- | Calculates SHA256 hash of the block
calculateHash :: Block -> BS.ByteString
calculateHash = sha256Hash . pack . show

-- | Checks if the proof of work for the previous block is valid. Computationally intensive.
isValidProof :: BS.ByteString -- ^ Previous block hash
             -> Int           -- ^ Proof of work
             -> Int           -- ^ Difficulty
             -> Bool          -- ^ `True` if proof is valid, `False` otherwise
isValidProof blockHash proof difficulty = BS.take difficulty (sha256Hash guess) == pattern
  where guess = blockHash `BS.append` pack (show proof)
        pattern = pack $ replicate difficulty '0'


-- | Calculates Proof of Work for the provided block hash and mining difficulty
calculateProofOfWork :: BS.ByteString -> Int -> Int
calculateProofOfWork blockHash difficulty = calculateProofOfWork_ 0
  where
    calculateProofOfWork_ guess
      | isValidProof blockHash guess difficulty = guess
      | otherwise                               = calculateProofOfWork_ (guess + 1)
