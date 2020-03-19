module Bitset (cardinality, empty, delete, insert, isEmpty, member, toList) where

import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Maybe
import Data.Sequence (Seq, Seq(Empty), adjust', ViewL((:<)), (|>), (><), singleton, takeWhileL)
import qualified Data.Sequence as Sequence (lookup)

data Bitset = Bitset { bitWords :: Seq Int } deriving (Eq, Show)

empty :: Bitset
empty = Bitset $ singleton 0

getWordIndex :: Int -> Int
getWordIndex number = shiftR number 5

addToWord :: Int -> Int -> Int
addToWord number word = word .|. shiftL 1 number

removeFromWord :: Int -> Int -> Int
removeFromWord number word = word .&. (complement $ shiftL 1 number)

expandSeqWith :: Int -> a -> Seq a -> Seq a
expandSeqWith targetLength fillWith seq
  | targetLength - length seq <= 0 = seq
  | otherwise = expandSeqWith targetLength fillWith $ seq |> fillWith

insert :: Bitset -> Int -> Bitset
insert bs number =
  let wordIndex     = getWordIndex number
      localNumber   = number - shiftL wordIndex 5
      expandedWords = expandSeqWith (wordIndex + 1) 0 $ bitWords bs
      newWords      = adjust' (addToWord localNumber) wordIndex expandedWords
  in bs { bitWords = newWords }

delete :: Bitset -> Int -> Bitset
delete bs number =
  let wordIndex    = getWordIndex number
      localNumber  = number - shiftL wordIndex 5
      newWords     = adjust' (removeFromWord localNumber) wordIndex $ bitWords bs
      trimmedWords = takeWhileL (> 0) newWords
  in bs { bitWords = trimmedWords }

member :: Bitset -> Int -> Bool
member bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      localWord   = Sequence.lookup wordIndex $ bitWords bs
      hasBit      = fmap (`testBit` localNumber) localWord
   in fromMaybe False hasBit

cardinality :: Bitset -> Int
cardinality bs = sum $ fmap popCount $ bitWords bs

wordToSequence :: Int -> Int -> [Int]
wordToSequence word wordIndex =
  let base = shiftL wordIndex 5
      setBits = filter (\x -> (word .&. (complement $ shiftL 1 x)) /= word) [0..31]
  in fmap (+ base) setBits

toListInternal :: [Int] -> Int -> [Int]
toListInternal words currentIndex =
  case words of
    (x : xs) -> wordToSequence x currentIndex ++ toListInternal xs (currentIndex + 1)
    [] -> []

toList :: Bitset -> [Int]
toList bs = toListInternal (Foldable.toList $ bitWords bs) 0

isEmpty :: Bitset -> Bool
isEmpty bs = all (== 0) $ bitWords bs
