-- |
-- Module      : Day06
-- Description : Solution to AOC 2022 Day 06: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/06>
module Day06 where

-- | Solution to Day 06.
main06 :: FilePath -> IO ()
main06 f = do
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
