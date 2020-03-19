module Bitset (empty, insert, member) where

import Data.Bits
import Data.Maybe
import Data.Sequence (Seq, adjust', (|>), singleton)
import qualified Data.Sequence as Sequence (lookup)

data Bitset = Bitset { bitWords :: Seq Int } deriving (Eq, Show)

empty :: Bitset
empty = Bitset $ singleton 0

getWordIndex :: Int -> Int
getWordIndex number = shiftR number 5

updateWord :: Int -> Int -> Int
updateWord number word = word .|. shiftL 1 number

expandSeqWith :: Int -> a -> Seq a -> Seq a
expandSeqWith targetLength fillWith seq
  | targetLength - length seq <= 0 = seq
  | otherwise = expandSeqWith targetLength fillWith $ seq |> fillWith

insert :: Bitset -> Int -> Bitset
insert bs number =
  let wordIndex     = getWordIndex number
      localNumber   = number - shiftL wordIndex 5
      expandedWords = expandSeqWith (wordIndex + 1) 0 $ bitWords bs
      newWords      = adjust' (updateWord localNumber) wordIndex expandedWords
  in bs { bitWords = newWords }

member :: Bitset -> Int -> Bool
member bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      localWord   = Sequence.lookup wordIndex $ bitWords bs
      hasBit      = fmap (`testBit` localNumber) localWord
   in fromMaybe False hasBit
