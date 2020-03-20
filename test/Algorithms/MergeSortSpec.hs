module MergeSortSpec (main, spec) where

import Test.Hspec
import Algorithms.MergeSort (mergeSort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "MergeSort.mergeSort" $ do
    it "sort a list using the given function" $ do
      mergeSort (<) [7,1,6,4,2,9] `shouldBe` [1,2,4,6,7,9]
      mergeSort (>) [7,1,6,4,2,9] `shouldBe` [9,7,6,4,2,1]
