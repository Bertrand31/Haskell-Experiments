module Main where

import Control.Exception (assert)
import MergeSort (mergeSort)
import GCD (gcd')

writeOk :: String -> IO ()
writeOk fnName = putStrLn $ fnName ++ " [OK]"

main :: IO ()
main = do
  assert (gcd' 48 64 == 16) writeOk "GCD"
  assert (mergeSort (\x y -> x < y) [7,1,6,4,2,9] == [1,2,4,6,7,9]) writeOk "MergeSort"
