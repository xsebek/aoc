-- |
-- Module      : Day19
-- Description : Solution to AOC 2020 Day 19: Monster Messages
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/19>
module Day19 where

-- | Solution to Day 19.
main19 :: FilePath -> IO ()
main19 f = do
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