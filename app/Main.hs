module Main (main) where

import System.Random
import Control.Exception (assert)
import MergeSort (mergeSort)
import GCD (gcd')
import qualified BloomFilter
import qualified Bitset

writeOk :: String -> IO ()
writeOk fnName = putStrLn $ fnName ++ " [OK]"

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
  let bitset = Bitset.empty
  let bitset1 = Bitset.insert bitset 5
  let bitset2 = Bitset.insert bitset1 50
  putStrLn $ show $ Bitset.member bitset2 5
  putStrLn $ show $ Bitset.member bitset2 50
  putStrLn $ show $ Bitset.member bitset2 32
  let bitset3 = Bitset.delete bitset2 50
  putStrLn $ show $ Bitset.member bitset3 50
