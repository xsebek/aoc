{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Day03
-- Description : Solution to AOC 2021 Day 03: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/03>
module Day03 where

import Data.Bits
import Data.Foldable (foldl')
import Data.Function ((&))

-- | Solution to Day 03.
main03 :: FilePath -> IO ()
main03 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> (Int, [Int])
parse s = (maximum (map length ls), map readB ls)
 where
  ls = lines s
  rb = \case
    '0' -> 0
    '1' -> 1
    _ -> error "not a binary"
  readB = sum . zipWith (*) (map bit [0 ..]) . map rb . reverse

-- Count of zero and one bits respectively
type C = (Int, Int)

countBIx :: Int -> Int -> C
countBIx num = b1 . testBit num
 where
  b1 b = if b then (0, 1) else (1, 0)

countB :: Int -> [C]
countB i = map (countBIx i) [0 ..]

countBs :: [Int] -> [C]
countBs = sumAll . map countB
 where
  sums :: [C] -> [C] -> [C]
  sums = zipWith (on2 (+))
  sumAll :: [[C]] -> [C]
  sumAll = foldl' sums (repeat (0, 0))

countBs' :: (Int, [Int]) -> [C]
countBs' (l, xs) = take l $ countBs xs

boolSet :: Bool -> Int -> Int -> Int
boolSet b = flip $ if b then setBit else clearBit

ray :: ((Int, Int) -> Bool) -> [C] -> Int
ray r = foldl' (&) 0 . zipWith (&) [0 ..] . map (boolSet . r)

gamma :: [C] -> Int
gamma = ray (uncurry (<=))

epsilon :: [C] -> Int
epsilon = ray (uncurry (>=))

-- >>> solve1 example
solve1 :: (Int, [Int]) -> Int
solve1 i = let c = countBs' i in gamma c * epsilon c

countsIx :: [Int] -> Int -> C
countsIx is ix = foldl' (on2 (+)) (0, 0) $ map (`countBIx` ix) is

filterBool :: Int -> Bool -> [Int] -> [Int]
filterBool i b = filter ((b ==) . (`testBit` i))

filterCriteria :: (Int -> Int -> Bool) -> (Int, [Int]) -> (Int, [Int])
filterCriteria _ (x, []) = error $ "No numbers to filter on position " <> show x
filterCriteria _ i@(_, [_]) = i
filterCriteria op (x, is) = (ix, filterBool ix (z `op` o) is)
 where
  (z, o) = is `countsIx` ix
  ix = x - 1

oxygenGen :: (Int, [Int]) -> Int
oxygenGen (x, is) = head . snd . (!! x) . iterate (filterCriteria (<=)) $ (x, is)

co2scrubber :: (Int, [Int]) -> Int
co2scrubber (x, is) = head . snd . (!! x) . iterate (filterCriteria (>)) $ (x, is)

-- >>> solve2 example
solve2 :: (Int, [Int]) -> Int
solve2 i = oxygenGen i * co2scrubber i

on2 :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
on2 f (a, b) (c, d) = (f a c, f b d)

example :: (Int, [Int])
example =
  parse . unlines $
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    ]