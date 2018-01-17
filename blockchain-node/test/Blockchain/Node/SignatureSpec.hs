module Blockchain.Node.SignatureSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Serialize as Serialize
import           Test.Hspec

import qualified Blockchain.Node.Signature as Signature


spec :: Spec
spec = do
    describe "ECDSA keys generation" $ do
        it "creates public/private pair and generates public from private" $ do
            (publicKey, privateKey) <- Signature.newKeyPair
            let regeneratedPublicKey = Signature.toPublic privateKey
            regeneratedPublicKey `shouldBe` publicKey
        it "creates signature and verifies it" $ do
            (publicKey, privateKey) <- Signature.newKeyPair
            let toSign = "a test string to sign ECDSA"
            signature <- Signature.sign privateKey toSign
            (Signature.verify publicKey signature toSign) `shouldBe` True
        it "creates deterministic address from public key" $ do
            (publicKey, _) <- Signature.newKeyPair
            let first = Signature.hashPublicKey publicKey
                second = Signature.hashPublicKey publicKey
            first `shouldBe` second

    describe "ECDSA public key serialization" $ do
        it "serializes and deserializes public key" $ do
            (publicKey, _) <- Signature.newKeyPair
            (Serialize.decode . Serialize.encode) publicKey `shouldBe` Right publicKey
        it "serializes to JSON and deserializes public key" $ do
            (publicKey, _) <- Signature.newKeyPair
            (Aeson.decode . Aeson.encode) publicKey `shouldBe` Just publicKey
