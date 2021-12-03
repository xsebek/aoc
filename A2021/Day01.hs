-- |
-- Module      : Day01
-- Description : Solution to AOC 2021 Day 01: Sonar Sweep
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/01>
module Day01 where

-- | Solution to Day 01.
main01 :: FilePath -> IO ()
main01 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Int]
parse = map read . lines

-- >>> solve1 example
-- 7
solve1 :: [Int] -> Int
solve1 ms = sum . map fromEnum $ zipWith (<) ms (tail ms)

-- >>> slide example
-- [607,618,618,617,647,716,769,792]
slide :: [Int] -> [Int]
slide ms = zipWith3 (\x y z -> x + y + z) ms (drop 1 ms) (drop 2 ms) 

-- >>> solve2 example
-- 5
solve2 :: [Int] -> Int
solve2 = solve1 . slide

example :: [Int]
example = 
  [ 199
  , 200
  , 208
  , 210
  , 200
  , 207
  , 240
  , 269
  , 260
  , 263
  ]
