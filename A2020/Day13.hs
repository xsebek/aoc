-- |
-- Module      : Day13
-- Description : Solution to AOC 2020 Day 13: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/13>
module Day13 where

-- | Solution to Day 13.
main13 :: FilePath -> IO ()
main13 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

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