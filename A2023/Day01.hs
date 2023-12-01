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

-- >>> solve2 example2
-- 281
solve2 :: [String] -> Int
solve2 = sum . map (read . firstAndLast . toDigits)
  where
    firstAndLast ds = [head ds, last ds]
    toDigits = \case
        (d : ts) | isDigit d -> d : toDigits ts
        ('z' : ts@('e' : 'r' : 'o' : _)) -> '0' : toDigits ts
        ('o' : ts@('n' : 'e' : _)) -> '1' : toDigits ts
        ('t' : ts@('w' : 'o' : _)) -> '2' : toDigits ts
        ('t' : ts@('h' : 'r' : 'e' : 'e' : _)) -> '3' : toDigits ts
        ('f' : ts@('o' : 'u' : 'r' : _)) -> '4' : toDigits ts
        ('f' : ts@('i' : 'v' : 'e' : _)) -> '5' : toDigits ts
        ('s' : ts@('i' : 'x' : _)) -> '6' : toDigits ts
        ('s' : ts@('e' : 'v' : 'e' : 'n' : _)) -> '7' : toDigits ts
        ('e' : ts@('i' : 'g' : 'h' : 't' : _)) -> '8' : toDigits ts
        ('n' : ts@('i' : 'n' : 'e' : _)) -> '9' : toDigits ts
        (_nonDigit : ts) -> toDigits ts
        [] -> []

example :: [String]
example =
    [ "1abc2"
    , "pqr3stu8vwx"
    , "a1b2c3d4e5f"
    , "treb7uchet"
    ]

example2 :: [String]
example2 =
    [ "two1nine"
    , "eightwothree"
    , "abcone2threexyz"
    , "xtwone3four"
    , "4nineeightseven2"
    , "zoneight234"
    , "7pqrstsixteen"
    ]
