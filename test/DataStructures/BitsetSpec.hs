module DataStructures.BitsetSpec (main, spec) where

import Test.Hspec
import DataStructures.Bitset as Bitset

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let emptyBs = Bitset.empty

  describe "Bitset.null" $ do
    it "should say an empty bitset is empty" $ do
      Bitset.null emptyBs `shouldBe` True

  describe "Bitset.insert" $ do
    it "should insert an integer into a bitset" $ do
      let bs = Bitset.insert emptyBs 5
      Bitset.size bs `shouldBe` (1 :: Int)

  describe "Bitset.insertMany" $ do
    it "should insert multiple integers into a bitset" $ do
      let bs = Bitset.insertMany emptyBs [5, 32, 1]
      Bitset.size bs `shouldBe` (3 :: Int)

  let populatedBs = Bitset.insertMany emptyBs [5, 31, 32, 62, 63, 64, 1030193]

  describe "Bitset.member" $ do
    it "should return true for numbers that have been inserted" $ do
      Bitset.member populatedBs 5 `shouldBe` True
      Bitset.member populatedBs 31 `shouldBe` True

    it "should return false for numbers that have not been inserted" $ do
      Bitset.member populatedBs 33 `shouldBe` False

  describe "Bitset.size" $ do
    it "should return the correct cardinality of the bitset" $ do
      Bitset.size populatedBs `shouldBe` (7 :: Int)

  describe "Bitset.toList" $ do
    it "should return a list of integers stored by the bitset" $ do
      Bitset.toList populatedBs `shouldBe` [5, 31, 32, 62, 63, 64, 1030193]

  describe "Bitset.delete" $ do
    it "should delete an integer from the bitset" $ do
      let bitsetMinus5 = Bitset.delete populatedBs 5
      Bitset.size bitsetMinus5 `shouldBe` (6 :: Int)
      Bitset.member bitsetMinus5 5 `shouldBe` False

  let bs1 = Bitset.insert Bitset.empty 8
  let bs2 = Bitset.insert Bitset.empty 16
  let bs3 = Bitset.empty

  describe "Bitset Monoid instance" $ do
    it "should merge bitsets, empty or not" $ do
      let bs4 = bs1 <> bs2 <> bs3
      Bitset.toList bs4 `shouldBe` [8, 16]
