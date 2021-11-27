-- |
-- Module      : Day22
-- Description : Solution to AOC 2020 Day 22: Crab Combat
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/22>
module Day22 where

-- | Solution to Day 22.
main22 :: FilePath -> IO ()
main22 f = do
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