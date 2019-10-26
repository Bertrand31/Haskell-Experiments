module BloomFilter (BloomFilter, create, empty, insert, mayContain) where

import System.Random (random, StdGen)
import qualified Data.BitSet as BitSet (BitSet, empty, insert, member, null)
import Data.Hashable (hash)

data BloomFilter = BloomFilter {
                                 nbOfItems :: Int,
                                 falsePositiveProbability :: Float,
                                 bitset :: BitSet.BitSet Int,
                                 maxSize :: Int,
                                 numberOfHashFunctions :: Int,
                                 hashSeed :: Int
                               } deriving (Eq, Show)

getMaxSize :: Int -> Float -> Int
getMaxSize n p =
  abs $ ceiling (fromIntegral n * (log p) / (log (1 / ((log 2) ^ 2))))

getNumberOfHashFunctions :: Int -> Int -> Int
getNumberOfHashFunctions n m =
  round (fromIntegral (m `div` n) * log 2)

create :: Int -> Float -> StdGen -> BloomFilter
create n p randGen =
  let m = getMaxSize n p
      k = getNumberOfHashFunctions n m
      hashSeed = fst $ random randGen
  in BloomFilter n p BitSet.empty m k hashSeed

empty :: BloomFilter -> Bool
empty = BitSet.null . bitset

insert :: Show a => a -> BloomFilter -> BloomFilter
insert elem bloomFilter =
  let str = show elem
      hashes = map (\x -> hash x `mod` (maxSize bloomFilter)) str
      newBitSet = Prelude.foldl (\bf x -> BitSet.insert x bf) (bitset bloomFilter) hashes
  in bloomFilter { bitset = newBitSet }

mayContain :: Show a => a -> BloomFilter -> Bool
mayContain elem bloomFilter =
  let str = show elem
      hashes = map (\x -> hash x `mod` (maxSize bloomFilter)) str
      bs = bitset bloomFilter
  in all (\x -> BitSet.member x bs) hashes

