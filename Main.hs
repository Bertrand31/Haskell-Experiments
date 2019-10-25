module Main where

import MergeSort
import GCD

main :: IO ()
main = do
  putStrLn (show $ gcd' 48 64)
  putStrLn (show $ mergeSort (\x y -> x < y) [7,1,6,4,2,9])
