module Main (main) where

import System.Random
import Control.Exception (assert)
import Lib
import MergeSort (mergeSort)
import GCD (gcd')
import qualified BloomFilter

writeOk :: String -> IO ()
writeOk fnName = putStrLn $ fnName ++ " [OK]"

main :: IO ()
main = do
  assert (gcd' 48 64 == 16) writeOk "GCD"
  assert (mergeSort (<) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
  gen <- getStdGen
  let bloomFilter = BloomFilter.create 100000 0.0001 gen
  assert (BloomFilter.empty bloomFilter == True) writeOk "BloomFilter empty"
  let bloomFilterWithFoo = BloomFilter.insert "foo" bloomFilter
  assert (BloomFilter.empty bloomFilterWithFoo == False) writeOk "BloomFilter empty"
  assert (BloomFilter.mayContain "foo" bloomFilterWithFoo == True) writeOk "BloomFilter mayContain"
  assert (BloomFilter.mayContain "bar" bloomFilterWithFoo == False) writeOk "BloomFilter mayContain"
