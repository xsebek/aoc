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

-- parse the numbers and their maximum length (highest bit)
parse :: String -> (Int, [Int])
parse s = (maximum (map length ls), map readB ls)
 where
  ls = lines s
  rb = \case
    '0' -> 0
    '1' -> 1
    _ -> error "not a binary"
  readB = sum . zipWith (*) (map bit [0 ..]) . map rb . reverse

data Count = C { zeros :: Int, ones :: Int}
 deriving (Eq, Show)

instance Semigroup Count where
  (C z1 o1) <> (C z2 o2) = C (z1 + z2) (o1 + o2)

instance Monoid Count where
  mempty = C 0 0

countBIx :: Int -> Int -> Count
countBIx num = b1 . testBit num
 where
  b1 b = if b then C 0 1 else C 1 0

countB :: Int -> [Count]
countB i = map (countBIx i) [0 ..]

countBs :: [Int] -> [Count]
countBs = sumAll . map countB
 where
  sumAll :: [[Count]] -> [Count]
  sumAll = foldl' sums (repeat mempty)
  sums = zipWith (<>)

countBs' :: (Int, [Int]) -> [Count]
countBs' (l, xs) = take l $ countBs xs

boolSet :: Bool -> Int -> Int -> Int
boolSet b = flip $ if b then setBit else clearBit

ray :: (Count -> Bool) -> [Count] -> Int
ray r = foldl' (&) 0 . zipWith (&) [0 ..] . map (boolSet . r)

gamma :: [Count] -> Int
gamma = ray (\(C z o) -> z <= o)

epsilon :: [Count] -> Int
epsilon = ray (\(C z o) -> z >= o)

-- >>> solve1 example
-- 198
solve1 :: (Int, [Int]) -> Int
solve1 i = let c = countBs' i in gamma c * epsilon c

countsIx :: [Int] -> Int -> Count
countsIx is ix = foldMap (`countBIx` ix) is

filterBool :: Int -> Bool -> [Int] -> [Int]
filterBool i b = filter ((b ==) . (`testBit` i))

-- filterCriteria (<=) example
-- (4,[30,22,23,21,28,16,25])
-- >>>  filterCriteria (>) example
-- (4,[4,15,7,2,10])
filterCriteria :: (Int -> Int -> Bool) -> (Int, [Int]) -> (Int, [Int])
filterCriteria _ (x, []) = error $ "No numbers to filter on position " <> show x
filterCriteria _ i@(_, [_]) = i
filterCriteria op (x, is) = (ix, filterBool ix (z `op` o) is)
 where
  (C z o) = is `countsIx` ix
  ix = x - 1

filterCriteria' :: (Int -> Int -> Bool) -> (Int, [Int]) -> Int
filterCriteria' op (x, is) = head . snd . (!! x) . iterate (filterCriteria op) $ (x, is)

-- >>> oxygenGen example
-- 23
oxygenGen :: (Int, [Int]) -> Int
oxygenGen = filterCriteria' (<=)

-- >>> co2scrubber example
-- 10
co2scrubber :: (Int, [Int]) -> Int
co2scrubber = filterCriteria' (>)

-- >>> solve2 example
-- 230
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