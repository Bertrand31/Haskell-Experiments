module DataStructures.Bitset (
  Bitset, size, empty, delete, insert, insertMany, member, DataStructures.Bitset.null, toList
) where

import Data.Bits ((.&.), (.|.), complement, popCount, shiftL, shiftR, testBit)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, adjust', dropWhileR)
import qualified Data.Sequence as S
import Utils (expandSeqWith, zipWith')

newtype Bitset = Bitset { bitWords :: Seq Int } deriving (Eq, Show)

instance Semigroup Bitset where
  a <> b = Bitset { bitWords = zipWith' (.|.) (bitWords a) (bitWords b) }

instance Monoid Bitset where
  mempty = empty

empty :: Bitset
empty = Bitset S.empty

getWordIndex :: Int -> Int
getWordIndex number = shiftR number 6

getLocalNumber :: Int -> Int -> Int
getLocalNumber wordIndex number = number - shiftL wordIndex 6

addToWord :: Int -> Int -> Int
addToWord number word = word .|. shiftL 1 number

removeFromWord :: Int -> Int -> Int
removeFromWord number word = word .&. complement (shiftL 1 number)

insert :: Bitset -> Int -> Bitset
insert bs number =
  let wordIndex     = getWordIndex number
      localNumber   = getLocalNumber wordIndex number
      expandedWords = expandSeqWith (wordIndex + 1) 0 $ bitWords bs
      newWords      = adjust' (addToWord localNumber) wordIndex expandedWords
  in  bs { bitWords = newWords }

insertMany :: Bitset -> [Int] -> Bitset
insertMany = foldl insert

delete :: Bitset -> Int -> Bitset
delete bs number =
  let wordIndex   = getWordIndex number
      localNumber = getLocalNumber wordIndex number
      newWords    = adjust' (removeFromWord localNumber) wordIndex $ bitWords bs
  in  bs { bitWords = dropWhileR (== 0) newWords }

member :: Bitset -> Int -> Bool
member bs number =
  let wordIndex   = getWordIndex number
      localNumber = getLocalNumber wordIndex number
      localWord   = S.lookup wordIndex $ bitWords bs
      hasBit      = (`testBit` localNumber) <$> localWord
  in  fromMaybe False hasBit

size :: Bitset -> Int
size = sum . (popCount <$>) . bitWords

wordToSequence :: Int -> Int -> [Int]
wordToSequence word wordIndex =
  let wordBase = shiftL wordIndex 6
      setBits = filter (testBit word) [0..63]
  in  (+ wordBase) <$> setBits

toIntList :: Int -> [Int] -> [Int]
toIntList index (x:xs) = wordToSequence x index ++ toIntList (index + 1) xs
toIntList _     []     = []

toList :: Bitset -> [Int]
toList = toIntList 0 . Foldable.toList . bitWords

null :: Bitset -> Bool
null = all (== 0) . bitWords
