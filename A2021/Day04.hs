-- |
-- Module      : Day04
-- Description : Solution to AOC 2021 Day 04: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/04>
module Day04 where

-- | Solution to Day 04.
main04 :: FilePath -> IO ()
main04 f = do
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