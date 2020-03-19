module BitsetSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import DataStructures.Bitset as Bitset

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let emptyBs = Bitset.empty

  describe "Bitset.insert" $ do
    it "should insert an integer into a bitset" $ do
      let bs = Bitset.insert emptyBs 5
      Bitset.size bs `shouldBe` (1 :: Int)

  describe "Bitset.insertMany" $ do
    it "should insert multiple integers into a bitset" $ do
      let bs = Bitset.insertMany emptyBs [5, 32, 1]
      Bitset.size bs `shouldBe` (3 :: Int)

  -- assert (gcd' 48 64 == 16) writeOk "GCD"
  -- assert (mergeSort (<) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
  -- gen <- getStdGen
  -- let bloomFilter = BloomFilter.empty 100000 0.0001 gen
  -- assert (BloomFilter.null bloomFilter) writeOk "BloomFilter null"
  -- let bloomFilterWithFoo = BloomFilter.insert "foo" bloomFilter
  -- assert (not $ BloomFilter.null bloomFilterWithFoo) writeOk "BloomFilter null"
  -- assert (BloomFilter.member "foo" bloomFilterWithFoo) writeOk "BloomFilter member"
  -- assert (not $ BloomFilter.member "bar" bloomFilterWithFoo) writeOk "BloomFilter member"
  -- let bitset = Bitset.empty
  -- assert (Bitset.null bitset) writeOk "Bitset null"
  -- assert (not $ Bitset.null bitset) writeOk "Bitset null"
  -- let bitset2 = Bitset.insertMany bitset [5, 31, 32, 62, 63, 64, 1030193]
  -- assert (Bitset.member bitset2 5) writeOk "Bitset member"
  -- assert (Bitset.member bitset2 31) writeOk "Bitset member"
  -- assert (not $ Bitset.member bitset2 33) writeOk "Bitset member"
  -- assert (Bitset.size bitset2 == 6) writeOk "Bitset size"
  -- assert (Bitset.toList bitset2 == [5, 31, 32, 62, 63, 64, 1030193]) writeOk "Bitset toList"
  -- let bitset3 = Bitset.delete bitset2 5
  -- assert (not $ Bitset.member bitset3 5) writeOk "Bitset delete"
  -- let bs1 = Bitset.insert Bitset.empty 8
  -- let bs2 = Bitset.insert Bitset.empty 16
  -- let bs3 = Bitset.empty
  -- let bs4 = bs1 <> bs2 <> bs3
  -- assert (Bitset.toList bs4 == [8, 16]) writeOk "Bitset monoid"
