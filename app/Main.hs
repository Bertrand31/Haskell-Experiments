module Main (main) where

import System.Random
import Control.Exception (assert)
import MineSweeper
import MergeSort (mergeSort)
import GCD (gcd')
import qualified BloomFilter

writeOk :: String -> IO ()
writeOk fnName = putStrLn $ fnName ++ " [OK]"

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x)) --prints strings in a column.


sampleInput = ["       ",
               " *     ",
               "    *  ",
               "   *   ",
               "      *",
               "***    ",
               "* *    ",
               "***    "]

main :: IO ()
main = do
  assert (gcd' 48 64 == 16) writeOk "GCD"
  assert (mergeSort (<) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
  gen <- getStdGen
  let bloomFilter = BloomFilter.empty 100000 0.0001 gen
  assert (BloomFilter.null bloomFilter == True) writeOk "BloomFilter null"
  let bloomFilterWithFoo = BloomFilter.insert "foo" bloomFilter
  assert (BloomFilter.null bloomFilterWithFoo == False) writeOk "BloomFilter null"
  assert (BloomFilter.member "foo" bloomFilterWithFoo == True) writeOk "BloomFilter member"
  assert (BloomFilter.member "bar" bloomFilterWithFoo == False) writeOk "BloomFilter member"
  pp $ minesweeper sampleInput
