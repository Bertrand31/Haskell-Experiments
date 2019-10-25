module Main where

import System.Random
import Control.Exception (assert)
import Lib
import MergeSort (mergeSort)
import GCD (gcd')

writeOk :: String -> IO ()
writeOk fnName = putStrLn $ fnName ++ " [OK]"

main :: IO ()
main = do
  assert (gcd' 48 64 == 16) writeOk "GCD"
  assert (mergeSort (\x y -> x < y) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
  gen <- getStdGen
  putStrLn $ show $ BloomFilter.create 100000 0.1
