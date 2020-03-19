module Bitset (empty, insert, member) where

import Data.Bits
import Data.Maybe
import qualified Data.Sequence as Sequence

data Bitset = Bitset { bitWords :: Sequence.Seq Int } deriving (Eq, Show)

empty :: Bitset
empty = Bitset $ Sequence.singleton 0

getWordIndex :: Int -> Int
getWordIndex number = shiftR number 5

updateWord :: Int -> Int -> Int
updateWord number word = word .|. shiftL 1 number

expandSeqWith :: Int -> a -> Sequence.Seq a -> Sequence.Seq a
expandSeqWith targetLength fillWith seq
  | targetLength - length seq <= 0 = seq
  | otherwise = expandSeqWith targetLength fillWith $ seq Sequence.|> fillWith

insert :: Bitset -> Int -> Bitset
insert bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      expandedWords = expandSeqWith (wordIndex + 1) 0 $ bitWords bs
      newWords    = Sequence.adjust' (updateWord localNumber) wordIndex expandedWords
  in bs { bitWords = newWords }

member :: Bitset -> Int -> Bool
member bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      localWord   = Sequence.lookup wordIndex $ bitWords bs
      hasBit      = fmap (`testBit` localNumber) localWord
   in fromMaybe False hasBit
