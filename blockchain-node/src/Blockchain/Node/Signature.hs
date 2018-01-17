{-# LANGUAGE DeriveAnyClass #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Blockchain.Node.Signature
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Elliptic Curve Digital Signature utility functions
--
-----------------------------------------------------------------------------

module Blockchain.Node.Signature (
    ECDSA.PublicKey
  , ECDSA.PrivateKey
  , KeyPair
  , newKeyPair

  , ECDSA.Signature
  , sign
  , verify

  , toPublic
  , hashPublicKey
) where

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding as Enc
import           GHC.Generics (Generic)

import qualified Blockchain.Node.Hash as Hash


-- | Use SECP256K1 curve
secp256k1 :: ECC.Curve
secp256k1 = ECC.getCurveByName ECC.SEC_p256k1


-- | A key pair
type KeyPair = (ECDSA.PublicKey, ECDSA.PrivateKey)


-- | Creates new elliptic curve key pair
newKeyPair :: IO KeyPair
newKeyPair = ECC.generate secp256k1


-- | Create a public key from a secret key
toPublic :: ECDSA.PrivateKey -> ECDSA.PublicKey
toPublic key = ECDSA.PublicKey curve point
  where
    curve  = ECDSA.private_curve key
    curve' = ECC.common_curve curve
    point  = ECC.pointMul curve (ECDSA.private_d key) g
    g      = ECC.ecc_g curve'

-- | SHA3_256 hashes a msg before signing
sign :: ECDSA.PrivateKey -> BS.ByteString -> IO ECDSA.Signature
sign pk = ECDSA.sign pk Hash.SHA3_256


-- | Verify a signature of a SHA3_256 encoded ByteString
verify :: ECDSA.PublicKey -> ECDSA.Signature -> BS.ByteString -> Bool
verify = ECDSA.verify Hash.SHA3_256


-- | Creates SHA256 hash of public key
hashPublicKey :: ECDSA.PublicKey -> Text
hashPublicKey pubkey = Hash.toText $ Hash.calculate (x, y)
  where
    ECC.Point x y = ECDSA.public_q pubkey


deriving instance Generic ECDSA.Signature
deriving instance Serialize ECDSA.Signature
instance ToJSON ECDSA.Signature
instance FromJSON ECDSA.Signature

deriving instance Generic ECDSA.PublicKey
instance Serialize.Serialize ECDSA.PublicKey
instance ToJSON ECDSA.PublicKey where
  toJSON = toJSON . Enc.decodeUtf8 . B64.encode . Serialize.encode

instance FromJSON ECDSA.PublicKey where
  parseJSON (Aeson.String v) = decodeForJSON v
  parseJSON invalid = Aeson.typeMismatch "ECDSA.PublicKey" invalid


decodeForJSON :: Text -> Aeson.Parser ECDSA.PublicKey
decodeForJSON text = case B64.decode $ Enc.encodeUtf8 text of
  Left invalid -> Aeson.typeMismatch "ECDSA.PublicKey" (Aeson.String $ pack invalid)
  Right v -> case Serialize.decode v of
    Left invalid -> Aeson.typeMismatch "ECDSA.PublicKey" (Aeson.String $ pack invalid)
    Right o -> return o


deriving instance Generic ECC.Curve
instance Serialize.Serialize ECC.Curve

deriving instance Generic ECC.CurveBinary
instance Serialize.Serialize ECC.CurveBinary

deriving instance Generic ECC.CurvePrime
instance Serialize.Serialize ECC.CurvePrime

deriving instance Generic ECC.CurveCommon
instance Serialize.Serialize ECC.CurveCommon

deriving instance Generic ECC.Point
instance Serialize.Serialize ECC.Point

