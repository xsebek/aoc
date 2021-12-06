{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Day06
-- Description : Solution to AOC 2021 Day 06: Lanternfish
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/06>
module Day06 where

import Data.IntMap qualified as Map
import Data.Vector qualified as V
import Linear ( (!*!), (*!) )
import Linear.V ( V(V) )

-- | Solution to Day 06.
main06 :: FilePath -> IO ()
main06 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Int]
parse = read . ('[' :) . (<> "]")

type School = V 9 Int

type Cycle = V 9 (V 9 Int)

count :: [Int] -> School
count is = V . V.fromList . map snd $ Map.toDescList (cs `Map.union` nulls)
 where
  cs = Map.fromListWith (+) $ zip is (repeat 1)
  nulls = Map.fromAscList $ zip [0 .. 8] (repeat 0)

vec9FromList :: [a] -> V 9 a
vec9FromList = V . V.fromList

-- | Markov chain for one day.
--
-- >>> mapM_ (print . toVector) day
-- [0,1,0,0,0,0,0,0,0]
-- [0,0,1,0,0,0,0,0,0]
-- [0,0,0,1,0,0,0,0,0]
-- [0,0,0,0,1,0,0,0,0]
-- [0,0,0,0,0,1,0,0,0]
-- [0,0,0,0,0,0,1,0,0]
-- [0,0,0,0,0,0,0,1,0]
-- [0,0,0,0,0,0,0,0,1]
-- [1,0,1,0,0,0,0,0,0]
day :: Cycle
day =
  vec9FromList
    [ vec9FromList
      [ fromEnum (x == y - 1 || (x, y) `elem` day0)
      | x <- [8, 7 .. 0]
      ]
    | y <- [8, 7 .. 0]
    ]
 where
  day0 :: [(Int, Int)]
  day0 = [(8, 0), (6, 0)]

powNN :: Cycle -> Int -> Cycle
powNN x n
  | n < 0 = error "Day cycle is not fractional ;)" -- luInv x `powNN` n
  | n == 0 = ident
  | otherwise = f x (n -1) x
 where
  ident :: Cycle
  ident = vec9FromList [vec9FromList [fromEnum (i == j) | i <- [(0 :: Int) .. 8]] | j <- [0 .. 8]]
  f :: Cycle -> Int -> Cycle -> Cycle
  f _ 0 y = y
  f a d y = g a d
   where
    g b i
      | even i = g (b !*! b) (i `quot` 2)
      | otherwise = f b (i -1) (b !*! y)

-- >>> solve1 example
-- 5934
solve1 :: [Int] -> Int
solve1 fish = sum $ count fish *! (day `powNN` 80)

-- >>> solve2 example
-- 26984457539
solve2 :: [Int] -> Int
solve2 fish = sum $ count fish *! (day `powNN` 256)

example :: [Int]
example = parse "3, 4, 3, 1, 2"