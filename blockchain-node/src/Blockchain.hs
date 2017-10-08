-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The blockchain datamodel + core functions
--
-----------------------------------------------------------------------------

module  Blockchain (
    Blockchain(..)
  , Block(..)
  , Transaction(..)
  , newBlockchain
  , addNewBlock

  , calculateProofOfWork
  , calculateHash
  , getLength
  , isValidProof
  , newTransaction
) where

import Control.Monad.State (State, get, put, modify)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)


-- | Current state of the blockchain. Contains transactions and list of blocks
data Blockchain = Blockchain {
  currentTransactions     :: ![Transaction],  -- ^ Current transactions (to be included in the next block)
  blocks                  :: ![Block]         -- ^ The list of valid blocks
} deriving (Show, Eq)


-- | A building block of the blockchain
data Block = Block {
  index           :: !Int,            -- ^ Position in the blockchain
  previousHash    :: !BS.ByteString,  -- ^ Hash of the previous block
  timestamp       :: !UTCTime,        -- ^ Block creation time
  transactions    :: ![Transaction],  -- ^ List of transactions within the block
  proof           :: !Int             -- ^ Proof of work
} deriving (Show, Eq)


-- | Represents single transaction between two adresses
data Transaction = Transaction {
  sender      :: !Text,   -- ^ Sender address
  recipient   :: !Text,   -- ^ Recipient address
  amount      :: !Int     -- ^ Transferred amount
} deriving (Show, Eq)


-- | Creates new blockchain with the genesis block
newBlockchain :: Blockchain
newBlockchain = Blockchain [] [genesisBlock]


-- | Returns current length of the blockchain
getLength :: State Blockchain Int
getLength = length <$> blocks <$> get


-- | The first block in the blockchain
genesisBlock :: Block
genesisBlock = Block {
  index = 0,
  previousHash = pack "1",
  timestamp = UTCTime (fromGregorian 2017 10 1) (secondsToDiffTime 0),
  transactions = [],
  proof = 100
}


-- | Adds new transaction to the transactions list, which will be added to the next block
newTransaction :: Text                  -- ^ Sender address
               -> Text                  -- ^ Recipient address
               -> Int                   -- ^ Transfer amount
               -> State Blockchain Int  -- ^ Blockchain along with the index of the next-to-be-mined block
newTransaction sender recipient amount = do
  blockchain <- get
  let transactions = currentTransactions blockchain
      newTransactions = transactions ++ [Transaction sender recipient amount]
  put blockchain { currentTransactions = newTransactions}
  getLength


-- | Returns the last block from the blockchain
getLastBlock :: State Blockchain Block
getLastBlock = last <$> blocks <$> get


-- | Adds new block to the blockchain
addNewBlock :: Int                            -- ^ Proof of work, must be valid
            -> Maybe BS.ByteString            -- ^ Optional hash of the previous block, if not provided, will be computed
            -> UTCTime                        -- ^ Creation time
            -> State Blockchain (Maybe Block) -- ^ Modified blockchain and new block if the proof is valid, otherwise `Nothing`
addNewBlock newBlockProof previousBlockHash creationTime = do
  lastBlockCalculatedHash <- calculateHash <$> getLastBlock
  let lastBlockHash = fromMaybe lastBlockCalculatedHash previousBlockHash

  if isValidProof lastBlockHash newBlockProof then do
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


-- | Calculates SHA256 hash of the block
-- TODO replace this with JSON serialization with ordered keys, instead of show
calculateHash :: Block -> BS.ByteString
calculateHash = SHA256.hash . pack . show

-- | Checks if the proof of work for the previous block is valid. Computationally intensive.
isValidProof :: BS.ByteString -- ^ Previous block hash
             -> Int           -- ^ Proof of work
             -> Bool          -- ^ `True` if proof is valid, `False` otherwise
isValidProof blockHash proof = BS.take 3 (SHA256.hash guess) == pack "000"
  where guess = blockHash `BS.append` pack (show proof)


-- | Calculates Proof of Work for the provided block hash
calculateProofOfWork :: BS.ByteString -> Int
calculateProofOfWork blockHash = calculateProofOfWork_ 0
  where
    calculateProofOfWork_ guess
      | isValidProof blockHash guess  = guess
      | otherwise                     = calculateProofOfWork_ (guess + 1)

