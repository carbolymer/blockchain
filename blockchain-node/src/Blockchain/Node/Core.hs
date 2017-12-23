{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Core
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The blockchain datamodel + core functions
--
-----------------------------------------------------------------------------

module  Blockchain.Node.Core (
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
  , Node(..)

  -- ** Constructor functions
  , newBlockchain
  , mineNewBlock
  , addTransaction
  , newTransaction

  -- * Utility functions
  , addNodes
  , thisNode
  , getLength
  , calculateProofOfWork
  , isValidBlock
  , isValidChain
  , isValidTransaction
  , validateAndUpdateChain
  , calculateHash
  , sha256Hash
  , (<$$>)
) where


import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO, readTVar, writeTVar)
import           Control.Monad (filterM, unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State (get)
import           Control.Monad.RWS (RWST, evalRWST, execRWST, liftIO, runRWST)
import           Control.Monad.Reader (ask)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import           Data.Foldable (foldr')
import           Data.List ((\\), foldl', sortBy, tail)
import           Data.Ord (Ordering(..))
import qualified Data.Set as S
import           Data.Text (Text, replace, pack)
import qualified Data.Text.Encoding as E
import           Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)
import           Data.Time.Calendar (fromGregorian)
import qualified Data.UUID as U
import           GHC.Generics (Generic)
import           Network.HostName (getHostName)
import           Prelude hiding (id)
import           System.Random (randomIO)

import           Blockchain.Node.Config (BlockchainConfig(..))
import           Logger
import           NetworkUtil (isValidUrl)

infoL, warningL :: (MonadIO m) => String -> m ()
[infoL, warningL] = getLogger "Blockchain.Node.Core" [INFO, WARNING]

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
  currentTransactions :: TVar [Transaction],  -- ^ Current transactions (to be included in the next block)
  blocks              :: TVar [Block],        -- ^ The list of valid blocks
  uuid                :: !Text,               -- ^ This node UUID
  nodes               :: TVar (S.Set Node),   -- ^ Nodes set
  hostName            :: !String              -- ^ This node hostname
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
  amount      :: !Int,     -- ^ Transferred amount
  time        :: !UTCTime
} deriving (Ord, Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- | Represents single blockchain node
data Node = Node {
  id  :: !Text, -- ^ UUID of the node
  url :: !Text  -- ^ URL under which the node is accessible
} deriving (Ord, Show, Generic)

instance ToJSON Node
instance FromJSON Node
instance Eq Node where
  -- id is unique
  a == b = (id a) == (id b)


-- | Creates new blockchain with the genesis block
newBlockchain :: IO Blockchain
newBlockchain = do
  nodeUuid <- U.toText <$> (randomIO :: IO U.UUID)
  currentTransactions <- newTVarIO []
  blocks <- newTVarIO [genesisBlock]
  nodes <- newTVarIO S.empty
  hostName <- getHostName
  return $ Blockchain currentTransactions blocks (replace "-" "" nodeUuid) nodes hostName


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
               -> Int                   -- ^ Transfer amount
               -> UTCTime               -- ^ Transaction time
               -> BlockchainApp Int     -- ^ Blockchain along with the index of the next-to-be-mined block
newTransaction sender recipient amount time = addTransaction $ Transaction sender recipient amount time


-- | Adds new transaction to the transactions list, which will be added to the next block
-- returns blockchain along with the index of the next-to-be-mined block
addTransaction :: Transaction -> BlockchainApp Int
addTransaction transaction = do
  transactions <- currentTransactions <$> get
  liftIO $ atomically $ modifyTVar' transactions (++ [transaction])
  infoL $ "added transaction: " ++ (show transaction)
  getLength


-- | Returns the last block from the blockchain
getLastBlock :: BlockchainApp Block
getLastBlock = liftIO =<< last <$$> readTVarIO <$> blocks <$> get


-- | Mines new block in the blockchain
mineNewBlock :: UTCTime                     -- ^ Creation time
             -> BlockchainApp (Maybe Block) -- ^ Modified blockchain and the new block if the block can be attached to
                                            -- the blockchain, otherwise the blockchain is not modified and Nothing is
                                            -- is returned
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

  infoL "Mining block..."
  -- actual mining here
  difficulty <- miningDifficulty <$> ask
  let validBlock = calculateProofOfWork newBlock difficulty
  -- seq is necessary for correct timestamp in log here
  (proof validBlock) `seq` infoL $ "Mined block, proof=" ++ (show $ proof validBlock)

  liftIO $ atomically $ do
    -- check if the blockchain was not was not modified during mining
    freshLastBlockHash <- calculateHash <$> last <$$> readTVar $ blocks blockchain
    if freshLastBlockHash == previousHash validBlock
        then do
          -- remove transactions already in newly mined block
          modifyTVar' (currentTransactions blockchain) (\\ transactions)
          -- add  block to the chain
          modifyTVar' (blocks blockchain) (++ [validBlock])
          return $ Just validBlock
        else return Nothing


-- | Returns transaction with the mining reward assigned to the current node
getMiningReward :: UTCTime -> BlockchainApp Transaction
getMiningReward time = do
  reward <- miningReward <$> ask
  recipientAddress <- uuid <$> get
  return $ Transaction "0" recipientAddress reward time

-- | Adds nodes to the node list in the blockchain
-- Returns `True` when
addNodes :: [Node] -> BlockchainApp ()
addNodes nodesToAdd = do
  blockchain <- get
  --  do not allow to insert current node
  filteredNodes <- filterM (isValidNode blockchain) nodesToAdd
  liftIO $ atomically $ modifyTVar' (nodes blockchain) $ \currentNodes -> do
    foldr' S.insert currentNodes filteredNodes
  infoL $ "added nodes: " ++ (show filteredNodes)
  where
    isValidNode :: Blockchain -> Node -> BlockchainApp Bool
    isValidNode blockchain node = do
      urlCheckResult <- isValidUrl (url node)
      unless urlCheckResult $ warningL $ "Invalid node received: " ++ (show node)
      return $ (id node /= uuid blockchain) && urlCheckResult

-- | Retrieves `Node` instance for current node
thisNode :: BlockchainConfig -> Blockchain -> Node
thisNode cfg blockchain = Node
    (uuid blockchain)
    (pack $ "http://" ++ hostName blockchain ++ ":" ++ (show $ httpPort cfg) ++ "/")

-- | Calculates SHA256 hash of the provided argument
sha256Hash :: BS.ByteString -> BS.ByteString
sha256Hash = B16.encode . SHA256.hash


-- | Calculates SHA256 hash of the block
calculateHash :: (Show a) => a -> BS.ByteString
calculateHash = sha256Hash . C8.pack . show


-- | Checks if the block is valid i.e. its proof of work meets difficulty requirements
isValidBlock :: (Show a) => a -- ^ Block to validate
             -> Int           -- ^ Difficulty
             -> Bool          -- ^ `True` if the block is valid, `False` otherwise
isValidBlock block difficulty = BS.take difficulty (calculateHash block) == pattern
  where pattern = C8.pack $ replicate difficulty '0'


-- | Calculates Proof of Work for the provided block and mining difficulty. Returns the new valid block with the updated
-- proof of work
calculateProofOfWork :: Block -> Int -> Block
calculateProofOfWork block difficulty
      | isValidBlock block difficulty = block
      | otherwise                     = calculateProofOfWork (block { proof = proof block + 1}) difficulty


-- | Validates provided chain of blocks
--
-- * Checks if blocks have correct hashes
-- * Validates all transactions in the blocks: (TODO)
--
--     1. No negative account balance (TODO)
--     2. Only last transaction in the block contains reward from mining (TODO)
--
isValidChain :: Int -> [Block] -> Bool
isValidChain _ []               = False
isValidChain _ (singleBlock:[]) = singleBlock == genesisBlock
isValidChain difficulty blocks  = do
  let blockPairs = zip blocks (tail blocks)
  foldl' validateBlockPair True blockPairs
  where
    validateBlockPair isValid (first, second) = isValid
         && (index second - index first == 1)
         && (previousHash second == calculateHash first)
         && isValidBlock second difficulty


-- | Validates list of blockchains. If the longest valid one is longer than the chain in this node, the current chain is
-- updated to the new one. Returns `True` when chain was updated, `False` otherwise
validateAndUpdateChain :: [[Block]] -> BlockchainApp Bool
validateAndUpdateChain []     = return False
validateAndUpdateChain [_:[]] = return False
validateAndUpdateChain chains = do
  difficulty <- miningDifficulty <$> ask
  currentChainTVar <- blocks <$> get
  -- longestFirst
  let sortedChains = sortBy descOrder chains

  liftIO $ atomically $ do
    currentChainLength <- length <$> readTVar currentChainTVar
    case getLongestValidChain difficulty sortedChains of
       Just chain | length chain > currentChainLength -> do
            writeTVar currentChainTVar chain
            return True
       _ -> return False
  where
    -- returns result of comparison for sorting in descending order
    descOrder chain1 chain2 = case length chain1 - length chain2 of
                               x | x > 0 -> LT
                               x | x < 0 -> GT
                               _         -> EQ

    getLongestValidChain difficulty sortedChains = case filter (isValidChain difficulty) sortedChains of
        []  -> Nothing
        x:_ -> Just x


-- | Validates transaction (that it does not produce negative amount)
isValidTransaction :: Transaction -> BlockchainApp Bool
isValidTransaction = undefined


-- | Applies function to the double functor
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f g = (f <$>) <$> g

infixl 4 <$$>
