--------------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Account
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Account model - a flattened view of blockchain
--
--------------------------------------------------------------------------------
module Blockchain.Node.Account (
    Account(..)
  , AccountsByAddress
  , toAccountsByAddress
  , getOrCreate
  , insertIntoMap
  , isValid
  , isNotGeneratingNegativeBalance
  , validateTransactions
  , (<+|)
) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Foldable (foldl', foldr')
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Blockchain.Node.Transaction (Transaction)
import qualified Blockchain.Node.Transaction as T


data Account = Account {
  accountId           :: !Text,
  balance             :: !Int,
  accountTransactions :: ![Transaction]
} deriving (Eq, Show, Generic)

instance ToJSON Account
instance FromJSON Account


type AccountsByAddress = Map.Map Text Account


-- | Checks if the balance is >= 0
isValid :: Account -> Bool
isValid = (>= 0) . balance


isNotGeneratingNegativeBalance :: Transaction -> AccountsByAddress -> Bool
isNotGeneratingNegativeBalance transaction accountMap
  = isValidRecipient && isValidSender
    where
      isValidRecipient  = isValid (recipientAccount <+| transaction)
      recipientAccount  = getOrCreate (T.recipient $ T.operation transaction) accountMap

      isValidSender     = case T.sender $ T.operation transaction of
        Nothing ->  True -- mining reward
        Just _  ->  isValid (senderAccount <+| transaction)
      senderAccount     = getOrCreate (fromJust $ T.sender $ T.operation transaction) accountMap


toAccountsByAddress :: [Transaction] -> AccountsByAddress
toAccountsByAddress = foldr' insertIntoMap Map.empty


getOrCreate :: Text -> AccountsByAddress -> Account
getOrCreate accId accountMap = do
  let account = fromMaybe
        (Account accId 0 [])
        (Map.lookup accId accountMap)
  if accountId account == accId
    then account
    else error $ "INVALID APP STATE, RETURNED ACCOUNT: " ++ (show $ accountId account)
            ++ " INSTEAD OF " ++ (show $ accId)



insertIntoMap :: Transaction -> AccountsByAddress -> AccountsByAddress
insertIntoMap transaction accountMap = addToTarget . removeFromSource $ accountMap
  where
    addToTarget :: AccountsByAddress -> AccountsByAddress
    addToTarget = Map.insert recipientId (recipientAccount <+| transaction)

    recipientId = T.recipient $ T.operation transaction
    recipientAccount = getOrCreate recipientId accountMap

    removeFromSource :: AccountsByAddress -> AccountsByAddress
    removeFromSource = case T.sender $ T.operation transaction of
      Nothing       -> id -- reward comes from teh void
      Just senderId
        -> Map.insert senderId (senderAccount <+| transaction)
        where
          senderAccount = getOrCreate senderId accountMap

(<+|) :: Account -> Transaction -> Account
(<+|) account transaction
  | isOutgoing && isIncoming
      = account {
          accountTransactions = accountTransactions account ++ [transaction]
        }
  | isOutgoing
      = account {
          balance             = balance account - transactionAmount,
          accountTransactions = accountTransactions account ++ [transaction]
        }
  | isIncoming
      = account {
          balance             = balance account + transactionAmount,
          accountTransactions = accountTransactions account ++ [transaction]
        }
  | otherwise
      = account
    where
      isOutgoing = case T.sender $ T.operation transaction of
        Nothing       -> False
        Just senderId -> accountId account == senderId
      isIncoming = accountId account == (T.recipient . T.operation) transaction
      transactionAmount = (T.amount . T.operation) transaction


-- | Validates transaction:
-- 1. Checks if the signature is correct
-- 1. that it does not produce negative in the provided accounts
validateTransactions :: [Transaction]     -- ^ transactions to validate
                     -> AccountsByAddress -- ^ accounts with valid transactions
                     -> [Transaction]     -- ^ valid transactions
validateTransactions inputTransactions accountsByAddress = fst $ foldl'
        collectValidTransactions
        ([], accountsByAddress)
        inputTransactions


-- | Validates transaction:
-- 1. Checks if the signature is correct
-- 1. that it does not produce negative amount in the chain
collectValidTransactions :: ([Transaction], AccountsByAddress)  -- ^ already validated transactions
                         -> Transaction                         -- ^ transaction to validate
                         -> ([Transaction], AccountsByAddress)  -- ^ confirmed transactions ++ validated transaction
collectValidTransactions (confirmedTransactions, accounts) transaction
  | isValidTransaction  = (confirmedTransactions ++ [transaction], insertIntoMap transaction accounts)
  | otherwise           = (confirmedTransactions, accounts)
  where
    isValidTransaction = isNotGeneratingNegativeBalance transaction accounts
                      && T.verify transaction


