module GCD where

getGCD :: Int -> Int -> Int
getGCD biggest smallest
  | rest == 0 = smallest
  | otherwise = getGCD smallest rest
  where rest = biggest `mod` smallest

gcd' :: Int -> Int -> Int
gcd' x y
  | x == y = x
  | otherwise = getGCD (max x y) (min x y)

