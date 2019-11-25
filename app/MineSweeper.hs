module MineSweeper where

import Data.Char

type Result = [String]

mineChar :: Char
mineChar = '*'

isMine :: Char -> Bool
isMine = (== mineChar)

minesweeper :: Result -> Result
minesweeper result = map (\row -> map ((!! 0) . show . isMine) row) result
