{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Day04
-- Description : Solution to AOC 2022 Day 04: Camp Cleanup
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/04>
module Day04 where
import Parser (Parser)
import Parser qualified as P
import Data.Text qualified as T

-- | Solution to Day 04.
main04 :: FilePath -> IO ()
main04 f = do
  input <- P.parseFile parser f
  print $ solve1 input
  print $ solve2 input

-- >>> P.parseExample' parser "2-4,6-8\n"
-- [(I {lower = 2, upper = 4},I {lower = 6, upper = 8})]
parser :: Parser [(Interval, Interval)]
parser = ((,) <$> parserInterval <* P.string "," <*> parserInterval) `P.endBy` P.eol 

parserInterval :: Parser Interval
parserInterval = I <$> P.decimal <*> (P.string "-" *> P.decimal)

data Interval = I {lower :: Int, upper:: Int}
 deriving (Eq, Ord, Show)

-- >>> solve1 example
-- 2
solve1 :: [(Interval, Interval)] -> Int
solve1 = length . filter fullContainment

-- >>> filter fullContainment example
-- [(I {lower = 2, upper = 8},I {lower = 3, upper = 7}),(I {lower = 6, upper = 6},I {lower = 4, upper = 6})]
fullContainment :: (Interval, Interval) -> Bool
fullContainment p = uncurry isContainedBy p || uncurry (flip isContainedBy) p

isContainedBy :: Interval -> Interval -> Bool
isContainedBy (I l1 u1) (I l2 u2) = l2 <= l1 && u1 <= u2

-- >>> solve2 example
-- 4
solve2 :: [(Interval, Interval)] -> Int
solve2 = length . filter (uncurry overlaps)

isInside :: Int -> Interval -> Bool
isInside x (I l u) = l <= x && x <= u 

overlaps :: Interval -> Interval -> Bool
overlaps i1@(I l1 u1) i2 = fullContainment (i1, i2) || l1 `isInside` i2 || u1 `isInside` i2

example :: [(Interval, Interval)]
example = P.parseExample' parser . T.unlines $
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]
