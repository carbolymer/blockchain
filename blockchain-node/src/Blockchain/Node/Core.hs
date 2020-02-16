{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

module  Blockchain.Node.Core (
  -- * Blockchain Application execution functions
    BlockchainApp
  , runApp

  -- * Blockchain data model
  -- ** Model
  , NodeState(..)
  , Block(..)
  , Node(..)

  -- ** Constructor functions
  , newNodeState
  , mineNewBlock
  , addTransaction
  , newTransaction
  , getMiningReward
  , getConfirmedTransactions

  -- * Utility functions
  , addNodes
  , thisNode
  , getLength
  , calculateProofOfWork
  , isValidBlock
  , isValidChain
  , validateAndUpdateChain
  , (<$$>)
) where


import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar',
                 readTVarIO, readTVar, writeTVar)
import           Control.Monad (filterM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Foldable (foldr')
import           Data.List ((\\), foldl', sortBy, tail)
import           Data.Ord (Ordering(..))
import qualified Data.Set as S
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Data.Time.Calendar (fromGregorian)
import           Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import           GHC.Generics (Generic)
import           Network.HostName (getHostName)
import           Prelude hiding (id)

import qualified Blockchain.Node.Account as Acc
import           Blockchain.Node.Config (BlockchainConfig(..))
import qualified Blockchain.Node.Hash as Hash
import qualified Blockchain.Node.MemPool as MemPool
import qualified Blockchain.Node.Signature as Signature
import           Blockchain.Node.Transaction (Transaction)
import qualified Blockchain.Node.Transaction as T
import           Logger
import           NetworkUtil (isValidUrl)

infoL, warningL :: (MonadIO m) => String -> m ()
[infoL, warningL] = getLogger "Blockchain.Node.Core" [INFO, WARNING]


-- | Blockchain application environment
type BlockchainApp = ReaderT NodeState IO

-- | Evaluates computation over an application, and returns final value
runApp :: NodeState         -- ^ a blockchain node state
       -> BlockchainApp a   -- ^ Reader computation
       -> IO a              -- ^ final value
runApp = flip runReaderT

-- | Current state of this node.
data NodeState = NodeState {
  memPool   :: TVar MemPool.MemPool,  -- ^ Current transactions (to be validated and included in the
                                      -- next block)
  blocks    :: TVar [Block],          -- ^ The list of valid blocks
  uuid      :: !Text,                 -- ^ This node UUID
  nodes     :: TVar (S.Set Node),     -- ^ Nodes set
  hostName  :: !String,               -- ^ This node hostname
  keyPair   :: !Signature.KeyPair,    -- ^ an ECDSA key pair used in signing of transactions
  config    :: !BlockchainConfig      -- ^ Blockchain config
} deriving (Eq)


-- | A building block of the blockchain
data Block = Block {
  index           :: !Int,              -- ^ Position in the blockchain
  previousHash    :: !(Hash.Hash Block),-- ^ Hash of the previous block
  timestamp       :: !UTCTime,          -- ^ Block creation time
  transactions    :: ![Transaction],    -- ^ List of transactions within the block
  proof           :: !Int               -- ^ Proof of work
} deriving (Show, Eq, Generic, Serialize)

instance ToJSON Block
instance FromJSON Block

-- | Represents single blockchain node
data Node = Node {
  id  :: !Text, -- ^ UUID of the node
  url :: !Text  -- ^ URL under which the node is accessible
} deriving (Show, Generic)

instance ToJSON Node
instance FromJSON Node
instance Eq Node where
  -- id is unique
  a == b = (id a) == (id b)
instance Ord Node where
  compare a b = compare (id a) (id b)


-- | Creates new blockchain node state with the genesis block
newNodeState :: BlockchainConfig -> IO NodeState
newNodeState cfg = do
  newMemPool <- newTVarIO mempty
  blocks <- newTVarIO [genesisBlock]
  nodes <- newTVarIO S.empty
  hostName <- getHostName
  keyPair@(publicKey, _) <- Signature.newKeyPair
  return $ NodeState {
        memPool = newMemPool,
        blocks = blocks,
        uuid = Signature.hashPublicKey publicKey,
        nodes = nodes,
        hostName = hostName,
        keyPair = keyPair,
        config = cfg}


-- | Returns current length of the blockchain
getLength :: BlockchainApp Int
getLength = liftIO =<< length <$$> readTVarIO <$> asks blocks


-- | The first block in the blockchain
genesisBlock :: Block
genesisBlock = Block {
  index = 0,
  previousHash = "1",
  timestamp = UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0),
  transactions = [],
  proof = 100
}


-- | Creates new transaction, signs it and adds to the MemPool
newTransaction :: Text                  -- ^ Recipient address
               -> Int                   -- ^ Transfer amount
               -> UTCTime               -- ^ Transaction time
               -> BlockchainApp Int     -- ^ Blockchain along with the index of the next-to-be-mined block
newTransaction recipient amount time = do
  sender <- asks uuid
  (pubKey, privKey) <- keyPair <$> ask
  let newOperation = T.Transfer sender recipient amount time
  operationSignature <- liftIO $ Signature.sign privKey (Serialize.encode newOperation)
  addTransaction $ T.Transaction
      pubKey
      operationSignature
      newOperation


-- | Adds new transaction to the MemPool, returns the index of the next-to-be-mined block
addTransaction :: Transaction -> BlockchainApp Int
addTransaction transaction = do
  memPool' <- asks memPool
  liftIO $ atomically $ modifyTVar' memPool' (MemPool.addTransaction transaction)
  infoL $ "added transaction: " ++ (show transaction)
  getLength


getConfirmedTransactions :: BlockchainApp [Transaction]
getConfirmedTransactions = do
  nodeState <- ask
  blocksInBlockchain <- liftIO $ readTVarIO $ blocks nodeState
  return $ concat $ map transactions blocksInBlockchain


-- | Returns the last block from the blockchain
getLastBlock :: BlockchainApp Block
getLastBlock = liftIO =<< last <$$> readTVarIO <$> asks blocks


-- | Mines new block in the blockchain
mineNewBlock :: UTCTime                     -- ^ Creation time
             -> BlockchainApp (Maybe Block) -- ^ Modified blockchain and the new block if the block
                                            -- can be attached to the blockchain, otherwise the
                                            -- blockchain is not modified and Nothing is returned
mineNewBlock creationTime = do
  lastBlockHash <- Hash.calculate <$> getLastBlock
  nodeState <- ask
  inputTransactions <- liftIO
      $   MemPool.unMemPool
      <$$> readTVarIO
      $   memPool nodeState
  newBlockIndex <- getLength
  accountsByAddress <- Acc.toAccountsByAddress <$> getConfirmedTransactions
  -- verify signatures
  let validTransactions = Acc.validateTransactions inputTransactions accountsByAddress
  infoL $ "Transactions " ++ (show $ length validTransactions) ++ " / "
        ++ (show $ length inputTransactions) ++ " from mempool are valid"
  infoL $ "Removed transactions: " ++ (show $ inputTransactions \\ validTransactions)
  rewardTransaction <- getMiningReward creationTime
  let newBlock = Block {
    index = newBlockIndex,
    previousHash = lastBlockHash,
    timestamp = creationTime,
    transactions = validTransactions ++ [rewardTransaction],
    proof = 0 -- initial value, correct one will be calculated later
  }

  -- actual mining here
  difficulty <- miningDifficulty <$> asks config
  infoL $ "Mining block with difficulty " ++ (show difficulty) ++ "..."
  let validBlock = calculateProofOfWork newBlock difficulty
  -- seq is necessary for correct timestamp in log here
  (proof validBlock) `seq` infoL $ "Mined block, proof=" ++ (show $ proof validBlock)

  liftIO $ atomically $ do
    -- check if the blockchain was not was not modified during mining
    freshLastBlockHash <- Hash.calculate <$> last <$$> readTVar $ blocks nodeState
    if freshLastBlockHash == previousHash validBlock
        then do
          -- remove transactions already in newly mined block
          modifyTVar'
              (memPool nodeState)
              (MemPool.removeTransactions inputTransactions)
          -- add  block to the chain
          modifyTVar' (blocks nodeState) (++ [validBlock])
          return $ Just validBlock
        else return Nothing


-- | Returns transaction with the mining reward assigned to the current node
getMiningReward :: UTCTime -> BlockchainApp Transaction
getMiningReward time = do
  reward <- asks (miningReward . config)
  recipientAddress <- asks uuid
  let newOperation = T.Reward recipientAddress reward time
  (pubKey, privKey) <- asks keyPair
  operationSignature <- liftIO $ Signature.sign privKey (Serialize.encode newOperation)
  return $ T.Transaction pubKey operationSignature newOperation


-- | Adds nodes to the node list in the blockchain
addNodes :: [Node] -> BlockchainApp ()
addNodes nodesToAdd = do
  nodeState <- ask
  --  do not allow to insert current node
  filteredNodes <- filterM (isValidNode nodeState) nodesToAdd
  liftIO $ atomically $ modifyTVar' (nodes nodeState) $ \currentNodes -> do
    foldr' S.insert currentNodes filteredNodes
  infoL $ "added nodes: " ++ (show filteredNodes)
  where
    isValidNode :: NodeState -> Node -> BlockchainApp Bool
    isValidNode nodeState node = do
      isValidNodeUrl <- isValidUrl (url node)
      unless isValidNodeUrl $ warningL $ "Invalid node received: " ++ (show node)
      return $ (id node /= uuid nodeState) && isValidNodeUrl


-- | Retrieves `Node` instance for current node
thisNode :: BlockchainConfig -> NodeState -> Node
thisNode cfg nodeState = Node
    (uuid nodeState)
    (pack $ "http://" ++ hostName nodeState ++ ":" ++ (show $ httpPort cfg) ++ "/")


-- | Checks if the block is valid i.e. its proof of work meets difficulty requirements
isValidBlock :: (Serialize a) => a -- ^ Block to validate
             -> Int                -- ^ Difficulty
             -> Bool               -- ^ `True` if the block is valid, `False` otherwise
isValidBlock block difficulty = Hash.validateDifficulty difficulty $ Hash.calculate block


-- | Calculates Proof of Work for the provided block and mining difficulty. Returns the new valid block with the updated
-- proof of work
calculateProofOfWork :: Block -> Int -> Block
calculateProofOfWork block difficulty
      | isValidBlock block difficulty = block
      | otherwise                     = calculateProofOfWork (block { proof = proof block + 1}) difficulty


-- | Validates provided chain of blocks
--
-- * Checks if blocks have correct hashes
-- * Validates all transactions in the blocks:
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
         && (previousHash second == Hash.calculate first)
         && isValidBlock second difficulty


-- | Validates list of blockchains. If the longest valid one is longer than the chain in this node, the current chain is
-- updated to the new one. Returns `True` when chain was updated, `False` otherwise
validateAndUpdateChain :: [[Block]] -> BlockchainApp Bool
validateAndUpdateChain []     = return False
validateAndUpdateChain [_:[]] = return False
validateAndUpdateChain chains = do
  difficulty <- asks (miningDifficulty . config)
  currentChainTVar <- asks blocks
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

-- | Applies function to the double functor
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f g = (f <$>) <$> g

infixl 4 <$$>
