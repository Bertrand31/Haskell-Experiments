module DataStructures.BloomFilterSpec (main, spec) where

import System.Random
import Test.Hspec
import DataStructures.BloomFilter as BloomFilter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  gen <- runIO getStdGen
  let bloomFilter = BloomFilter.empty 100000 0.0001 gen

  describe "BloomFilter.null" $ do
    it "should say an empty bloom filter is empty" $ do
      BloomFilter.null bloomFilter `shouldBe` True

  let bloomFilterWithFoo = BloomFilter.insert bloomFilter "foo"

  describe "BloomFilter.member" $ do
    it "should return true for an already-inserted member" $ do
      BloomFilter.member bloomFilterWithFoo "foo" `shouldBe` True

    it "should (probably) return false for a non-existing member" $ do
      BloomFilter.member bloomFilterWithFoo "bar" `shouldBe` False
      BloomFilter.member bloomFilterWithFoo "baz" `shouldBe` False
