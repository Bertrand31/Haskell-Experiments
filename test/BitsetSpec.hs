module BitsetSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Bitset

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
