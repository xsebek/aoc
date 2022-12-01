-- |
-- Module      : Day01
-- Description : Solution to AOC 2022 Day 01: Calorie Counting
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/01>
module Day01 where
import Data.List.Split (splitWhen)
import Data.List (sort)

-- | Solution to Day 01.
main01 :: FilePath -> IO ()
main01 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [[Int]]
parse = map (read <$>) . splitWhen null . lines

-- >>> solve1 example
-- 24000
solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

-- >>> solve2 example
-- 45000
solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum

example :: [[Int]]
example = parse . unlines $
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]
