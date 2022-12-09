-- |
-- Module      : Day08
-- Description : Solution to AOC 2022 Day 08: Treetop Tree House
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/08>
module Day08 where
import qualified Data.List as List
import Data.Char (ord, chr)
import Data.Array (Array, array, bounds, (!), indices)
import Data.Bifunctor (first)

-- | Solution to Day 08.
main08 :: FilePath -> IO ()
main08 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [[Int]]
parse = map (map $ read . (:[])) . lines

-- >>> solve1 example
-- 21
solve1 :: [[Int]] -> Int
solve1 = length . filter id . concat . visibleGrid

visibleGrid :: [[Int]] -> [[Bool]]
visibleGrid g = List.zipWith4 (List.zipWith4 any4) (r g) (t g) (l g) (b g)
 where
  mVis = map (visible (-1))
  mRev = map reverse
  r = mVis
  t = List.transpose . mVis . List.transpose
  l = mRev . mVis . mRev
  b = List.transpose . mRev . mVis . mRev . List.transpose
  any4 u v x y = u || v || x || y

visible :: Int -> [Int] -> [Bool]
visible h = \case
  [] -> []
  t:ts -> (h < t) : visible (max h t) ts

-- >>> solve2 example
-- 8
solve2 :: [[Int]] -> Int
solve2 gs = let ga = grid gs in maximum . map (scenic ga) $ indices ga 

type Grid a = Array (Int, Int) a

grid :: [[a]] -> Grid a
grid g = array (boundGrid g) $ indexGrid g

boundGrid :: [[a]] -> ((Int, Int), (Int, Int))
boundGrid g = ((0, 0), (pred . length $ head g, pred $ length g))

indexGrid :: [[c]] -> [((Int, Int), c)]
indexGrid = concat . zipWith (\y -> map (first (,y))) [0..] .  map (zip [0..])

scenic :: Grid Int -> (Int, Int) -> Int
scenic g (x,y) = product . map vis $ scenics g (x,y)
  where
   h = g ! (x,y)
   vis d = (\l -> l + fromEnum (l /= length d)) . length . takeWhile (h>) $ map (g!) d

scenics :: Grid Int -> (Int, Int) -> [[(Int, Int)]]
scenics g (x,y) = [ir, il, it, ib]
  where
   (mx, my) = snd $ bounds g
   ir = tail $ map (,y) [x..mx]
   ib = tail $ map (x,) [y..my]
   il = tail . reverse $ map (,y) [0..x]
   it = tail . reverse $ map (x,) [0..y]

prettyFilter :: [[Bool]] -> [[Int]] -> [[Char]]
prettyFilter = zipWith (zipWith p)
 where
  p b t = if b then chr (ord '0' + t) else '.'

example :: [[Int]]
example = parse . unlines $
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]
