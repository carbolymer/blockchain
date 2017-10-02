module  Blockchain (
      Blockchain(..)
    , Block(..)
    , Transaction(..)
    , newBlockchain
    , addNewBlock
    , calculateHash
    , getLength
    , newTransaction
) where

import Control.Monad.State (State, get, put, modify)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)


-- | Current state of the blockchain. Contains transactions and list of blocks
data Blockchain = Blockchain {
    currentTransactions     :: ![Transaction],  -- ^ Current transactions (to be included in the next block
    blocks                  :: ![Block]         -- ^ The list of valid blocks
} deriving (Show, Eq)


-- | A building block of the blockchain
data Block = Block {
    index           :: !Int,            -- ^ Position in the blockchain
    previous_hash   :: !ByteString,     -- ^ Hash of the previous block
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
    previous_hash = pack "1",
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


-- | Adds new block to the blockchain unconditionally
addNewBlock :: Int                      -- ^ Proof of work
            -> Maybe ByteString         -- ^ Optional hash of the previous block, if not provided, will be computed
            -> UTCTime                  -- ^ Creation time
            -> State Blockchain Block   -- ^ Modified blockchain and new block
addNewBlock newBlockProof previousBlockHash creationTime = do
    blockchain <- get
    newBlockIndex <- getLength
    lastBlockHash <- calculateHash <$> getLastBlock
    let newBlock = Block {
            index = newBlockIndex,
            previous_hash = fromMaybe lastBlockHash previousBlockHash,
            timestamp = creationTime,
            transactions = currentTransactions blockchain,
            proof = newBlockProof
        }
    -- update state
    put blockchain {
        currentTransactions = [],
        blocks = (blocks blockchain) ++ [newBlock]
    }
    return newBlock


-- | Calculates SHA256 hash of the block
calculateHash :: Block -> ByteString
calculateHash = SHA256.hash . pack . show


