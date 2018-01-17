{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Transaction
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Transactions model
--
--------------------------------------------------------------------------------
module Blockchain.Node.Transaction (
  -- * Data types
    Transaction(Transaction)
  , Operation(Reward, Transfer)
  , NewTransactionRequest(..)
  -- * Transaction getters
  , operation
  , pubKey
  , signature
  , sender
  , recipient
  , amount
  , time

  -- * transaction signature and validation
  , sign
  , verify
) where

import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import           Data.Serialize (Serialize, get, encode, put)
import           Data.Serialize.Text ()
import           GHC.Generics (Generic)

import qualified Blockchain.Node.Signature as Signature


-- | Represents single transaction between two adresses
data Transaction = Transaction {
  _pubKey      :: !Signature.PublicKey,  -- ^ Sender ECDSA public key
  _signature   :: !Signature.Signature,  -- ^ Operation ECDSA signature
  _operation   :: !Operation             -- ^ Operation details
} deriving (Show, Eq, Generic, Serialize)

pubKey :: Transaction -> Signature.PublicKey
pubKey = _pubKey

operation :: Transaction -> Operation
operation = _operation

signature :: Transaction -> Signature.Signature
signature = _signature


instance Ord Transaction where
  compare a b = compare (_operation a) (_operation b)
instance ToJSON Transaction
instance FromJSON Transaction


-- | Represents single transaction between two adresses
data Operation
    = Reward {
      _recipient   :: !Text,     -- ^ Recipient address
      _amount      :: !Int,      -- ^ Transferred amount
      _time        :: !UTCTime   -- ^ Transaction time
    }
    | Transfer {
      _sender      :: !Text,     -- ^ Sender address, has to be derived from the public key in the
                                 -- transaction
      _recipient   :: !Text,     -- ^ Recipient address
      _amount      :: !Int,      -- ^ Transferred amount
      _time        :: !UTCTime   -- ^ Transaction time
    } deriving (Show, Eq, Generic)

sender :: Operation -> Maybe Text
sender Reward {} = Nothing
sender t@Transfer {} = Just $ _sender t

recipient :: Operation -> Text
recipient = _recipient

amount :: Operation -> Int
amount = _amount

time :: Operation -> UTCTime
time = _time

instance Ord Operation where
  compare a b = compare (_time a) (_time b)
instance Serialize DiffTime where
  get = picosecondsToDiffTime <$> get
  put = put . diffTimeToPicoseconds
deriving instance Generic Day
instance Serialize Day
deriving instance Generic UTCTime
instance Serialize UTCTime
instance ToJSON Operation
instance FromJSON Operation
instance Serialize Operation


data NewTransactionRequest = NewTransactionRequest {
    newAmount    :: !Int,
    newSender    :: !Text,
    newRecipient :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON NewTransactionRequest
instance FromJSON NewTransactionRequest

-- | Signs the operation using private key
sign :: Signature.PrivateKey -> Operation -> IO Signature.Signature
sign privKey operation' = Signature.sign privKey (encode operation')


-- | Verifies the transaction signature
verify :: Transaction -> Bool
verify transaction = Signature.verify
    (pubKey transaction)
    (signature transaction)
    (encode $ operation transaction)
