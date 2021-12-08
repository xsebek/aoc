-- |
-- Module      : Day07
-- Description : Solution to AOC 2021 Day 07: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/07>
module Day07 where

-- | Solution to Day 07.
main07 :: FilePath -> IO ()
main07 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Int]
parse = read . ('[' :) . (<> "]")

fuelSum :: (Int -> Int) -> [Int] -> Int
fuelSum f xs = minimum $ map sumDs minToMax
  where
    minToMax = [minimum xs..maximum xs]
    dist x = f . abs . (-x+)
    sumDs x = sum $ map (dist x) xs

-- >>> solve1 example
-- 349812
solve1 :: [Int] -> Int
solve1 = fuelSum id

-- >>> solve2 example
-- 99763899
solve2 :: [Int] -> Int
solve2 = fuelSum (\i -> i * (i + 1) `div` 2)

example :: [Int]
example = [16,1,2,0,4,2,7,1,2,14]