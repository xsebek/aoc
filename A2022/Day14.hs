-- |
-- Module      : Day14
-- Description : Solution to AOC 2022 Day 14: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/14>
module Day14 where

-- | Solution to Day 14.
main14 :: FilePath -> IO ()
main14 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Int]
parse = map read . lines

-- >>> solve1 example
solve1 :: a -> Int
solve1 = errorWithoutStackTrace "Part 1 not implemented"

-- >>> solve2 example
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

example :: a
example = undefined
