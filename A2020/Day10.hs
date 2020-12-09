-- |
-- Module      : Day10
-- Description : Solution to AOC 2020 Day 10: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/10>
module Day10 where

import Criterion.Main (bench, bgroup, defaultMain, whnf)

-- | Solution to Day 9.
main10 :: FilePath -> IO ()
main10 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "10"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

parse :: String -> [Int]
parse = map read . lines

-- >>> solve1 example
solve1 :: a -> Int
solve1 = undefined

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

example :: a
example = undefined