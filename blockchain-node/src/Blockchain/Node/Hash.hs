--------------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Hash
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Hashing utilities
--
--------------------------------------------------------------------------------

module Blockchain.Node.Hash (
    CH.SHA3_256(..)
  , Hash
  , sha256
  , calculate
  , validateDifficulty
  , toText
  , fromText
) where


import qualified Crypto.Hash.Algorithms as CH
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import           Data.Data (Data, Typeable)
import           Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import           GHC.Generics (Generic)


newtype Hash a = Hash {
  unHash :: BS.ByteString
} deriving (Eq, Read, Show, Data, IsString, Serialize, Typeable, Ord, Generic)


-- | Calculates SHA256 hash of the provided argument
sha256 :: BS.ByteString -> Hash a
sha256 = Hash . B16.encode . SHA256.hash


-- | Calculates SHA256 hash of the show representation of argument
calculate :: (Serialize a) => a -> Hash a
calculate = sha256 . Serialize.encode


-- | validates if the hash matches provided difficulty i.e. leading characters (determined by difficulty) are '0'
validateDifficulty :: Int           -- ^ difficulty
                   -> Hash a        -- ^ hash to verify
                   -> Bool          -- ^ `True` if hash is valid, `False` otherwise
validateDifficulty difficulty hashToVerify = BS.take difficulty (unHash hashToVerify) == pattern
  where pattern = C8.pack $ replicate difficulty '0'

--------------------------------------------------------------------------------
-- JSON serialization
--------------------------------------------------------------------------------

instance ToJSON (Hash a) where
  toJSON = toJSON . toText

instance FromJSON (Hash a) where
  parseJSON (Aeson.String v) = fromText v
  parseJSON invalid = Aeson.typeMismatch "Hash" invalid


toText :: Hash a -> Text
toText = Enc.decodeUtf8 . unHash

fromText :: (Monad m) => Text -> m (Hash a)
fromText = (return . Hash) . Enc.encodeUtf8
