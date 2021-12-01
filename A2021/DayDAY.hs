-- |
-- Module      : DayDAY
-- Description : Solution to AOC 2021 Day DAY: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/DAY>
module DayDAY where

-- | Solution to Day DAY.
mainDAY :: FilePath -> IO ()
mainDAY f = do
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