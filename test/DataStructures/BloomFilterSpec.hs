module BloomFilterSpec (main, spec) where

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

  let bloomFilterWithFoo = BloomFilter.insert "foo" bloomFilter

  describe "BloomFilter.member" $ do
    it "should return true for an already-inserted member" $ do
      BloomFilter.member "foo" bloomFilterWithFoo `shouldBe` True

    it "should (probably) return false for a non-existing member" $ do
      BloomFilter.member "bar" bloomFilterWithFoo `shouldBe` False
  -- assert (mergeSort (<) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
