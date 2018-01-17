--------------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.MemPool
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- The Aggregate of the transactions to be confirmed
--
--------------------------------------------------------------------------------

module Blockchain.Node.MemPool (
    MemPool(..)
  , addTransaction
  , removeTransactions
) where

import           Data.List ((\\))
import           Data.Semigroup (Semigroup)
import           GHC.Generics (Generic)

import           Blockchain.Node.Transaction (Transaction)


-- | Contains transactions to be confirmed
newtype MemPool = MemPool {
  unMemPool :: [Transaction]  -- ^ List of transactions to confirm
} deriving (Show, Eq, Generic, Monoid, Semigroup)


-- | Adds transaction to the MemPool
addTransaction :: Transaction   -- ^ Transaction to add
               -> MemPool       -- ^ Target MemPool
               -> MemPool       -- ^ New MemPool with transaction
addTransaction transaction (MemPool pool) = MemPool (pool ++ [transaction])


-- | Removes transactions from MemPool
removeTransactions :: [Transaction] -- ^ Transactions to remove
                   -> MemPool       -- ^ Target MemPool
                   -> MemPool       -- ^ New MemPool without transactions
removeTransactions transactions (MemPool pool) = MemPool $ pool \\ transactions
