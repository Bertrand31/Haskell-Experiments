module BloomFilter where

-- import System.Random

-- data BloomFilter = BloomFilter {
                                 -- nbOfItems :: Int,
                                 -- falsePositiveProbability :: Float,
                                 -- bitset :: Bitset
                                 -- maxSize :: Int,
                                 -- numberOfHashFunctions :: Int,
                                 -- hashSeed :: Int
                               -- } deriving (Eq)

-- getMaxSize :: Int -> Float -> Int
-- getMaxSize nbOfItems falsePositiveProbability =
  -- ceil $ abs (nbOfItems * log falsePositiveProbability) / (log $ 1 / pow $ (log 2) 2)

-- getNumberOfHashFunctions :: Int -> Int -> Int
-- getNumberOfHashFunctions nbOfItems maxSize =
  -- round ((maxSize / nbOfItems) * log 2)

-- create :: Int -> Int -> Kek -> BloomFilter
-- create nbOfItems falsePositiveProbability randGen =
  -- let maxSize = getMaxSize nbOfItems falsePositiveProbability
      -- numberOfHashFunctions = getNumberOfHashFunctions nbOfItems maxSize
      -- hashSeed = random randGen
  -- in BloomFilter nbOfItems falsePositiveProbability Bitset.empty maxSize numberOfHashFunctions hashSeed
