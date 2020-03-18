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

insert :: Bitset -> Int -> Bitset
insert bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      newWords    = Sequence.adjust (updateWord localNumber) wordIndex $ bitWords bs
  in bs { bitWords = newWords }

member :: Bitset -> Int -> Bool
member bs number =
  let wordIndex   = getWordIndex number
      localNumber = number - shiftL wordIndex 5
      localWord   = Sequence.lookup wordIndex $ bitWords bs
      hasBit      = fmap (`testBit` localNumber) localWord
   in fromMaybe False hasBit
