-- |
-- Module      : Day01
-- Description : Solution to AOC 2023 Day 01: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2023/day/01>
module Day01 where
import Data.Char (isDigit)

-- | Solution to Day 01.
main01 :: FilePath -> IO ()
main01 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [String]
parse = lines

-- >>> solve1 example
-- 142
solve1 :: [String] -> Int
solve1 = sum . map (read . firstAndLast . filter isDigit)
  where
    firstAndLast ds = [head ds, last ds]

-- >>> solve2 example
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

example :: [String]
example =
  [ "1abc2"
  , "pqr3stu8vwx"
  , "a1b2c3d4e5f"
  , "treb7uchet"
  ]
