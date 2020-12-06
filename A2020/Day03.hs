-- | Solution to AOC 2020 Day 3: Toboggan Trajectory
--
-- https://adventofcode.com/2020/day/3
module Day03 where

import Control.Monad ((>=>))
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Solution

-- | Solution to day 3.
--
-- With the toboggan login problems resolved, you set off toward the
-- airport. While travel by toboggan might be easy, it\'s certainly not
-- safe: there\'s very minimal steering and the area is covered in trees.
-- You\'ll need to see which angles will take you near the fewest trees.
--
-- Due to the local geology, trees in this area only grow on exact integer
-- coordinates in a grid. You make a map (your puzzle input) of the open
-- squares (@.@) and trees (@#@) you can see. For example:
--
-- > ..##.......
-- > #...#...#..
-- > .#....#..#.
-- > ..#.#...#.#
-- > .#...##..#.
-- > ..#.##.....
-- > .#.#.#....#
-- > .#........#
-- > #.##...#...
-- > #...##....#
-- > .#..#...#.#
--
-- These aren\'t the only trees, though; due to something you read about
-- once involving arboreal genetics and biome stability, the same pattern
-- repeats to the right many times:
--
-- > ..##.........##.........##.......  --->
-- > #...#...#..#...#...#..#...#...#..
-- > .#....#..#..#....#..#..#....#..#.
-- > ..#.#...#.#..#.#...#.#..#.#...#.#
-- > .#...##..#..#...##..#..#...##..#.
-- > ..#.##.......#.##.......#.##.....  --->
-- > .#.#.#....#.#.#.#....#.#.#.#....#
-- > .#........#.#........#.#........#
-- > #.##...#...#.##...#...#.##...#...
-- > #...##....##...##....##...##....#
-- > .#..#...#.#.#..#...#.#.#..#...#.#  --->
--
-- You start on the open square (@.@) in the top-left corner and need to
-- reach the bottom (below the bottom-most row on your map).
--
-- The toboggan can only follow a few specific slopes (you opted for a
-- cheaper model that prefers rational numbers);
-- start by /counting all the trees/ you would encounter for the slope
-- /right 3, down 1/:
--
-- From your starting position at the top-left, check the position that is
-- right 3 and down 1. Then, check the position that is right 3 and down 1
-- from there, and so on until you go past the bottom of the map.
--
-- The locations you\'d check in the above example are marked here with @O@
-- where there was an open square and @X@ where there was a tree:
--
-- > ..##.........##.........##.......  --->
-- > #..O#...#..#...#...#..#...#...#..
-- > .#....X..#..#....#..#..#....#..#.
-- > ..#.#...#O#..#.#...#.#..#.#...#.#
-- > .#...##..#..X...##..#..#...##..#.
-- > ..#.##.......#.X#.......#.##.....  --->
-- > .#.#.#....#.#.#.#.O..#.#.#.#....#
-- > .#........#.#........X.#........#
-- > #.##...#...#.##...#...#.X#...#...
-- > #...##....##...##....##...#X....#
-- > .#..#...#.#.#..#...#.#.#..#...X.#  --->
solution :: Solution Forest
solution = solutionS toForest solve1 solve2

type Tree = Bool

type Forest = [[Tree]]

-- | A pair of horizontal and vertical distance.
type Dir = (Int, Int)

-- | Function to reverse 'toForest'.
--
-- >>> map (pretty . take 3) $ toForest "..#\n#.#"
-- ["..#","#.#"]
pretty :: [Tree] -> String
pretty = map (bool '.' '#')

-- >>> map (pretty . take 9) $ toForest "..#\n#.#"
-- ["..#..#..#","#.##.##.#"]
toForest :: String -> Forest
toForest = map (cycle . map (== '#')) . lines

-- | Part One.
--
-- In the above example, traversing the map using this slope would cause you
-- to encounter 7 trees.
--
-- Starting at the top-left corner of your map and following a slope of
-- right 3 and down 1, how many trees would you encounter?
--
-- >>> solve1 example
-- 7
solve1 :: Forest -> Int
solve1 = hits (3, 1)

hits :: Dir -> Forest -> Int
hits dir = length . filter id . ride dir

-- >>> pretty $ ride (3,1) example
-- "..#.##.####"
ride :: Dir -> Forest -> [Tree]
ride dir = catMaybes . takeWhile isJust . go
  where
    go = (listToMaybe >=> listToMaybe) `iterOn` move dir

move :: Dir -> Forest -> Forest
move (h, v) = map (drop h) . drop v

-- | Part Two.
--
-- Time to check the rest of the slopes - you need to minimize the
-- probability of a sudden arboreal stop, after all.
--
-- Determine the number of trees you would encounter if, for each of the
-- following slopes, you start at the top-left corner and traverse the map
-- all the way to the bottom:
--
-- -   Right 1, down 1.
-- -   Right 3, down 1. (This is the slope you already checked.)
-- -   Right 5, down 1.
-- -   Right 7, down 1.
-- -   Right 1, down 2.
--
-- In the above example, these slopes would find @2@, @7@, @3@, @4@, and
-- @2@ tree(s) respectively; multiplied together, these produce the answer
-- @336@.
--
-- __What do you get if you multiply together the number of trees
-- encountered on each of the listed slopes?__
--
-- >>> solve2 example
-- 336
solve2 :: Forest -> Int
solve2 = product . flip map dirs . flip hits
  where
    dirs :: [Dir]
    dirs = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

iterOn :: (t -> a) -> (t -> t) -> t -> [a]
iterOn f g x = f x : iterOn f g (g x)

example :: Forest
example =
  toForest . intercalate "\n" $
    [ "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    ]