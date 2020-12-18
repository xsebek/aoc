-- |
-- Module      : Day18
-- Description : Solution to AOC 2020 Day 18: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/18>
module Day18 where

-- | Solution to Day 18.
main18 :: FilePath -> IO ()
main18 f = do
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