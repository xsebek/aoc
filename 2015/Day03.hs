{-# LANGUAGE LambdaCase #-}

-- | Solution to AOC 2015 Day 3: Perfectly Spherical Houses in a Vacuum
--
-- https://adventofcode.com/2015/day/3
--
-- Santa is delivering presents to an infinite two-dimensional grid of houses.
--
-- He begins by delivering a present to the house at his starting location,
-- and then an elf at the North Pole calls him via radio and tells him where
-- to move next. Moves are always exactly one house to the north (^), south (v),
-- east (>), or west (<). After each move, he delivers another present to the
-- house at his new location.
--
-- However, the elf back at the north pole has had a little too much eggnog,
-- and so his directions are a little off, and Santa ends up visiting some
-- houses more than once.
--
-- How many houses receive at least one present?
module Day03 where

import Control.Arrow (first, second)
import Data.Maybe (fromJust)
import Data.Set (Set, insert, singleton, unions)

-- | Process input text file.
main :: IO ()
main = do
  input <- readFile "input03.txt"
  print (solve1 input)
  print (solve2 input)

type House = (Int, Int)

type Direction = Char

-- | Part One.
--
-- >>> solve1 $ parseDirection <$> ">"
-- 2
-- >>> solve1 $ parseDirection <$> "^>v<"
-- 4
-- >>> solve1 $ parseDirection <$> "^v^v^v^v^v"
-- 2
solve1 :: [Direction] -> Int
solve1 = length . allVisited

allVisited :: [Direction] -> Set House
allVisited ds = getHouses $ go (singleton start, ds, start)
  where
    start = (0, 0)
    getHouses (hs, _, _) = hs
    mvisit = (visits =<<)
    go = fromJust . until (null . mvisit) mvisit . Just

visits ::
  (Set House, [Direction], House) ->
  Maybe (Set House, [Direction], House)
visits (_, [], _) = Nothing
visits (hs, d : ds, h) = let nh = move d h in Just (insert nh hs, ds, nh)

move :: Direction -> House -> House
move = \case
  '^' -> first (+ 1)
  '>' -> second (+ 1)
  'v' -> first (subtract 1)
  '<' -> second (subtract 1)
  ch -> error $ "The character '" ++ [ch] ++ "' is not a valid direction!"

-- | Part Two.
--
-- The next year, to speed up the process, Santa creates a robot version
-- of himself, Robo-Santa, to deliver presents with him.
--
-- Santa and Robo-Santa start at the same location (delivering two presents
-- to the same starting house), then take turns moving based on instructions
-- from the elf, who is eggnoggedly reading from the same script as the
-- previous year.
--
-- This year, how many houses receive at least one present?
--
-- >>> solve2 $ parseDirection <$> "^v^v^v^v^v"
-- 11
solve2 :: [Direction] -> Int
solve2 ds = length $ unions $ allVisited <$> [s, r]
  where
    (s, r) = unzip $ pairs ds

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "The list has odd length"
pairs (l : r : xs) = (l, r) : pairs xs
