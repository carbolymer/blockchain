module Blockchain.Node.HashSpec where

import           Test.Hspec

import           Blockchain.Node.Config (BlockchainConfig(..), defaultConfig)
import           Blockchain.Node.Core (newNodeState, uuid, keyPair)
import qualified Blockchain.Node.Hash as Hash

testConfig :: BlockchainConfig
testConfig = defaultConfig { miningDifficulty = 2 }

spec :: Spec
spec = do
  describe "Test hashing" $ do
    it "hashes test string, compares with precalculated hash" $ do
      let testString = "testhash"
          testHash = "4bc75035d73f6083683e040fc31f28e0ec6d1cbce5cb0a5e2611eb89bceb6c16"
      Hash.sha256 testString `shouldBe` testHash

  describe "Test hash difficulty validation" $ do
    let hash = "000012345asdfbasdfbasdfbasdfbasdfbasdfbasdfbsdf" :: Hash.Hash String
    it "positive case" $ do
      (Hash.validateDifficulty 4 hash) `shouldBe` True
    it "negative case" $ do
      (Hash.validateDifficulty 5 hash) `shouldBe` False

  describe "Hash calculation" $ do
    it "calculates hash of serializable object" $ do
      nodeState <- newNodeState testConfig
      let (pubKey, _) = keyPair nodeState
      putStrLn $ show $ uuid nodeState
      putStrLn $ show $ Hash.calculate pubKey
      True `shouldBe` True


