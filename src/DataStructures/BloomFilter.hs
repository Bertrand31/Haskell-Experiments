module DataStructures.BloomFilter (empty, insert, member, DataStructures.BloomFilter.null) where

import System.Random (random, StdGen)
import qualified DataStructures.Bitset as Bitset
import Data.Hashable (hashWithSalt)

-- n: expected number of items in the Bloom Filter
-- p: acceptable probability of a false positive
-- m: max number of bits the Bloom Filter will use
-- k: number of hashing functions

data BloomFilter = BloomFilter {
  n :: Int, p :: Float, bitset :: Bitset.Bitset, m :: Int, k :: Int, hashSeed :: Int
} deriving (Eq, Show)

getMaxSize :: Int -> Float -> Int
getMaxSize n p = abs $ ceiling $ fromIntegral n * (log p) / (log (1 / (log 2 ^ 2)))

getNumberOfHashFunctions :: Int -> Int -> Int
getNumberOfHashFunctions n m = round $ fromIntegral (m `div` n) * log 2

empty :: Int -> Float -> StdGen -> BloomFilter
empty n p randGen =
  let m = getMaxSize n p
      k = getNumberOfHashFunctions n m
      hashSeed = fst $ random randGen
  in  BloomFilter n p Bitset.empty m k hashSeed

null :: BloomFilter -> Bool
null = Bitset.null . bitset

getHashes :: Show a => BloomFilter -> a -> [Int]
getHashes bloomFilter elem =
  let str     = show elem
      seed    = hashSeed bloomFilter
      maxSize = m bloomFilter
  in  (`mod` maxSize) . abs . (`hashWithSalt` str) . (seed +) <$> [1..(k bloomFilter)]

insert :: Show a => BloomFilter -> a -> BloomFilter
insert bloomFilter elem =
  let hashes    = getHashes bloomFilter elem
      newBitset = Bitset.insertMany (bitset bloomFilter) hashes
  in  bloomFilter { bitset = newBitset }

-- Returns whether an element *may be* present in the bloom filter.
-- This function can yield false positives, but not false negatives.
member :: Show a => BloomFilter -> a -> Bool
member bloomFilter elem =
  let hashes = getHashes bloomFilter elem
      bs     = bitset bloomFilter
  in  all (Bitset.member bs) hashes
