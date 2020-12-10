-- |
-- Module      : Day10
-- Description : Solution to AOC 2020 Day 10: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/10>
module Day10 where

import Data.List (foldl', sort)
import Optics

-- | Solution to Day 10.
main10 :: FilePath -> IO ()
main10 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Int]
parse = map read . lines

-- >>> solve1 example
-- 220
solve1 :: [Int] -> Int
solve1 = (\(d1, _, d3) -> d1 * d3) . count123

-- >>> count123 smallExample
-- (7,0,5)
count123 :: [Int] -> (Int, Int, Int)
count123 = foldr ((%!~ succ) . dif) (0, 0, 0) . difference
  where
    dif = \case
      1 -> _1
      2 -> _2
      3 -> _3
      n -> error $ "Wrong difference: " <> show n

-- >>> difference smallExample
-- [1,3,1,1,1,3,1,1,3,1,3,3]
difference :: [Int] -> [Int]
difference xs = let s = sort xs in zipWith (-) s (0 : s) <> [3]

-- >>> solve2 example
-- 19208
solve2 :: [Int] -> Int
solve2 = head . arrangements . difference

-- >>> difference smallExample
-- [1,3,1,1,1,3,1,1,3,1,3,3]
-- >>> arrangements $ difference smallExample
-- [8,0,0,8,8,0,0,8,4,4,0,0,4,2,1,1,0,0,1,1]
arrangements :: [Int] -> [Int]
arrangements = foldl' run [1] . init . map pred
  where
    run :: [Int] -> Int -> [Int]
    run results i = sum (take (3 - i) results) : replicate i 0 <> results

-- >>> sort smallExample
-- [1,4,5,6,7,10,11,12,15,16,19]
smallExample :: [Int]
smallExample =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]

-- >>> sort example
-- [1,2,3,4,7,8,9,10,11,14,17,18,19,20,23,24,25,28,31,32,33,34,35,38,39,42,45,46,47,48,49]
example :: [Int]
example =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]