-- |
-- Module      : Day09
-- Description : Solution to AOC 2022 Day 09: Rope Bridge
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/09>
module Day09 where

import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..), _x, _y)
import Optics (Lens', lensVL, view, (%~), (<|))

-- | Solution to Day 09.
main09 :: FilePath -> IO ()
main09 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Move]
parse = map (read . ("M " ++)) . lines

data Move = M Dir Int
  deriving (Eq, Show, Read)

data Dir
  = R
  | L
  | U
  | D
  deriving (Eq, Ord, Show, Read)

-- >>> solve1 example
-- 13
solve1 :: [Move] -> Int
solve1 = Set.size . visited . List.foldl' move (emptyS 2)

type P = V2 Int

x :: Lens' P Int
x = lensVL _x

y :: Lens' P Int
y = lensVL _y

data S = S {rope :: Seq P, visited :: Set P}
  deriving (Eq, Show)

ropeHead :: Seq P -> P
ropeHead = flip Seq.index 1

emptyS :: Int -> S
emptyS l = S (Seq.fromList $ replicate l 0) (Set.singleton 0)

move :: S -> Move -> S
move s = List.foldl' microMove s . discrete1Moves

discrete1Moves :: Move -> [Dir]
discrete1Moves (M d m) = replicate m d

microMove :: S -> Dir -> S
microMove (S r v) m = S nr (Set.insert nt v)
 where
  (h, ts) = case Seq.viewl r of
    Seq.EmptyL -> error "Can not move nonexistent rope!"
    (h' Seq.:< ts') -> (h', toList ts')
  nh =
    h & case m of
      R -> x %~ succ
      L -> x %~ pred
      U -> y %~ succ
      D -> y %~ pred
  nr = nh <| follow nh ts
  nt = case Seq.viewr nr of
    Seq.EmptyR -> error "impossible - rope has at least its head"
    (_rs Seq.:> nt') -> nt'
  follow _i [] = Seq.empty
  follow i@(V2 ix iy) (j@(V2 jx jy) : js) = nj <| follow nj js
   where
    nj
      | j `isTouching` i = j
      | jy == iy = j & x %~ (djx +)
      | jx == ix = j & y %~ (djy +)
      | otherwise = j + dj
    dj@(V2 djx djy) = diffSign j i

isTouching :: P -> P -> Bool
isTouching (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

isSameXOrY :: P -> P -> Bool
isSameXOrY (V2 x1 y1) (V2 x2 y2) = x1 == x2 || y1 == y2

-- >>> diffSign (V2 0 0) (V2 2 2)
-- V2 1 1
-- >>> diffSign (V2 0 0) (V2 (-4) 1)
-- V2 (-1) 1
diffSign :: P -> P -> P
diffSign (V2 x1 y1) (V2 x2 y2) = V2 (signum $ x2 - x1) (signum $ y2 - y1)

-- >>> solve2 example
solve2 :: [Move] -> Int
solve2 = Set.size . visited . List.foldl' move (emptyS 10)

-- >>> error . ("This is fine.\n" <>) . draw $ List.foldl' move (emptyS 2) example
-- *** Exception: This is fine.
-- ..##.
-- ...##
-- .H0##
-- ....#
-- ####.
draw :: S -> String
draw (S r v) = unlines [[drawC (V2 px py) | px <- [minX .. maxX]] | py <- reverse [minY .. maxY]]
 where
  h = ropeHead r
  drawC p =
    if p == h
      then 'H'
      else case p `Seq.elemIndexL` r of
        Just i -> chr (ord '0' + i)
        Nothing -> if p `Set.member` v then '#' else '.'
  xs = Set.map (view x) (Set.insert h v)
  ys = Set.map (view y) (Set.insert h v)
  (minX, maxX) = (Set.findMin xs, Set.findMax xs)
  (minY, maxY) = (Set.findMin ys, Set.findMax ys)

-- >>> example
-- [M R 4,M U 4,M L 3,M D 1,M R 4,M D 1,M L 5,M R 2]
example :: [Move]
example =
  parse . unlines $
    [ "R 4"
    , "U 4"
    , "L 3"
    , "D 1"
    , "R 4"
    , "D 1"
    , "L 5"
    , "R 2"
    ]

-- >>> error . ("This is fine.\n" <>) . draw $ List.foldl' move (emptyS 10) exampleLarge
-- *** Exception: This is fine.
-- H.....................
-- 2.....................
-- 3.....................
-- 4.....................
-- 5.....................
-- 6.....................
-- 7.....................
-- 8.....................
-- 9.....................
-- #.............###.....
-- #............#...#....
-- .#..........#.....#...
-- ..#..........#.....#..
-- ...#........#.......#.
-- ....#......#.........#
-- .....#..............#.
-- ......#............#..
-- .......#..........#...
-- ........#........#....
-- .........########.....
exampleLarge :: [Move]
exampleLarge =
  [ M R 5
  , M U 8
  , M L 8
  , M D 3
  , M R 17
  , M D 10
  , M L 25
  , M U 20
  ]
