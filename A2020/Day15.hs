-- |
-- Module      : Day15
-- Description : Solution to AOC 2020 Day 15: Rambunctious Recitation
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/15>
module Day15 where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import qualified Data.IntMap.Strict as Map

type Spoken = Map.IntMap Int

-- | Solution to Day 15.
main15 :: FilePath -> IO ()
main15 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "15"
        [ bench "part1" $ whnf solve1 input
        --, bench "part2" $ whnf solve2 input -- only try on powerful computer
        ]
    ]

-- >>> parse example
-- [0,3,6]
parse :: String -> [Int]
parse = map read . words . map (\c -> if c == ',' then ' ' else c)

type Ix = Int

-- >>> solve1 $ parse example
-- 436
solve1 :: [Int] -> Ix
solve1 = runUntil 2020

-- >>> runUntil 10 $ parse example
-- 0
runUntil :: Int -> [Int] -> Int
runUntil i input = newUntil i m (head rev)
  where
    rev = reverse $ zip input [1 ..]
    m = Map.fromList $ tail rev

newUntil :: Int -> Spoken -> (Int, Ix) -> Int
newUntil i sp (num, ix) = if six == i then nr else newUntil i nsp (nr, six)
  where
    !six = succ ix
    (!nsp, !nr) = nextNumber sp num ix

-- >>> nextNumber (Map.fromList [(0,1), (3,2)]) 6 3
-- (fromList [(0,1),(3,2),(6,3)],0)
nextNumber :: Spoken -> Int -> Ix -> (Spoken, Int)
nextNumber spoken !num !ix = (newSpoken, next)
  where
    !next = maybe 0 (ix -) (Map.lookup num spoken)
    !newSpoken = Map.insert num ix spoken

-- >>> solve2 $ parse example
-- ProgressCancelledException
solve2 :: [Int] -> Int
solve2 = runUntil 30000000

example :: String
example = "0,3,6"