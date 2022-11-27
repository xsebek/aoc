-- |
-- Module      : Day25
-- Description : Solution to AOC 2021 Day 25: Sea Cucumber
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/25>
module Day25 where

-- | Solution to Day 25.
main25 :: FilePath -> IO ()
main25 f = do
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
