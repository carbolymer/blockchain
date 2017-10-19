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
  , mineNewBlock
  , addTransaction
  , newTransaction

  -- * Utility functions
  , getLength
  , calculateProofOfWork
  , isValidBlock
  , calculateHash
  , sha256Hash
) where


import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO, readTVar, writeTVar)
import           Control.Monad.State (get, put, modify)
import           Control.Monad.RWS (RWST, evalRWST, execRWST, liftIO, runRWST)
import           Control.Monad.Reader (ask)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Char8 (pack)
import           Data.List ((\\))
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
-- TODO move Blockchain datatype into reader monad
type BlockchainApp = RWST BlockchainConfig () Blockchain IO

-- | Evaluates computation over an application, and returns final state
execApp :: BlockchainConfig -- ^ config
        -> BlockchainApp a  -- ^ RWS computation
        -> Blockchain       -- ^ a blockchain state
        -> IO Blockchain    -- ^ final state
execApp config blockchainApp  blockchain = fst <$> execRWST blockchainApp config blockchain

-- | Evaluates computation over an application, and returns final value
evalApp :: BlockchainConfig -- ^ config
        -> BlockchainApp a  -- ^ RWS computation
        -> Blockchain       -- ^ a blockchain state
        -> IO a             -- ^ final value
evalApp config blockchainApp blockchain = fst <$> evalRWST blockchainApp config blockchain

-- | Evaluates computation over an application, and returns final value
runApp :: BlockchainConfig    -- ^ config
       -> BlockchainApp a     -- ^ RWS computation
       -> Blockchain          -- ^ a blockchain state
       -> IO (a, Blockchain)  -- ^ final value and blockchain
runApp config blockchainApp blockchain = do
  (result, newBlockchain, _) <- runRWST blockchainApp config blockchain
  return (result, newBlockchain)

-- | Current state of the node.
data Blockchain = Blockchain {
  currentTransactions     :: TVar [Transaction],  -- ^ Current transactions (to be included in the next block)
  blocks                  :: TVar [Block],        -- ^ The list of valid blocks
  uuid                    :: !Text
} deriving (Eq)


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
  currentTransactions <- newTVarIO []
  blocks <- newTVarIO [genesisBlock]
  return $ Blockchain currentTransactions blocks (replace "-" "" nodeUuid)


-- | Returns current length of the blockchain
getLength :: BlockchainApp Int
getLength = liftIO =<< length <$$> readTVarIO <$> blocks <$> get


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
               -> Double                -- ^ Transfer amount
               -> BlockchainApp Int     -- ^ Blockchain along with the index of the next-to-be-mined block
newTransaction sender recipient amount = addTransaction $ Transaction sender recipient amount


-- | Adds new transaction to the transactions list, which will be added to the next block
-- returns blockchain along with the index of the next-to-be-mined block
addTransaction :: Transaction -> BlockchainApp Int
addTransaction transaction = do
  transactions <- currentTransactions <$> get
  liftIO $ atomically $ modifyTVar' transactions (++ [transaction])
  getLength


-- | Returns the last block from the blockchain
getLastBlock :: BlockchainApp Block
getLastBlock = liftIO =<< last <$$> readTVarIO <$> blocks <$> get


-- | Mines new block in the blockchain
mineNewBlock :: UTCTime             -- ^ Creation time
             -> BlockchainApp Block -- ^ Modified blockchain and the new block
mineNewBlock creationTime = do
  lastBlockHash <- calculateHash <$> getLastBlock
  blockchain <- get
  transactions <- liftIO $ readTVarIO $ currentTransactions blockchain
  newBlockIndex <- getLength
  rewardTransaction <- getMiningReward creationTime
  let newBlock = Block {
    index = newBlockIndex,
    previousHash = lastBlockHash,
    timestamp = creationTime,
    transactions = transactions ++ [rewardTransaction],
    proof = 0 -- initial value, correct one will be calculated later
  }
  liftIO $ putStrLn "Mining block..."
  -- actual mining here
  difficulty <- miningDifficulty <$> ask
  let validBlock = calculateProofOfWork newBlock difficulty
  liftIO $ putStrLn $ "Mined block, proof=" ++ (show $ proof validBlock)

  liftIO $ atomically $ do
    -- remove transactions already in newly mined block
    modifyTVar' (currentTransactions blockchain) (\\ transactions)
    -- add  block to the chain
    modifyTVar' (blocks blockchain) (++ [validBlock])

  return validBlock


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

-- | Checks if the block is valid
isValidBlock :: Block         -- ^ Block to validate
             -> Int           -- ^ Difficulty
             -> Bool          -- ^ `True` if the block is valid, `False` otherwise
isValidBlock block difficulty = BS.take difficulty (calculateHash block) == pattern
  where pattern = pack $ replicate difficulty '0'


-- | Calculates Proof of Work for the provided block and mining difficulty. Returns the new valid block with the updated
-- proof of work
calculateProofOfWork :: Block -> Int -> Block
calculateProofOfWork block difficulty
      | isValidBlock block difficulty = block
      | otherwise                     = calculateProofOfWork (block { proof = proof block + 1}) difficulty


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f g = (f <$>) <$> g
