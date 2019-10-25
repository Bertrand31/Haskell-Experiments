module Main where

getGCD :: Int -> Int -> Int
getGCD biggest smallest
  | rest == 0 = smallest
  | otherwise = getGCD smallest rest
  where rest = biggest `mod` smallest

gcd' :: Int -> Int -> Int
gcd' x y
  | x == y = x
  | otherwise = getGCD (max x y) (min x y)

main :: IO ()
main = do
  number1 <- getLine
  number2 <- getLine
  let gcd = gcd' (read number1 :: Int) (read number2 :: Int)
  putStrLn(show gcd)
