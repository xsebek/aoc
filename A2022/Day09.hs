-- |
-- Module      : Day09
-- Description : Solution to AOC 2022 Day 09: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/09>
module Day09 where

import Data.Function ((&))
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..), _x, _y)
import Optics (lensVL, view, (%~))
import Optics.Lens (Lens')

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
solve1 = Set.size . visited . List.foldl' move emptyS

type P = V2 Int

x :: Lens' P Int
x = lensVL _x

y :: Lens' P Int
y = lensVL _y

data S = S {ropeHead :: P, ropeTail :: P, visited :: Set P}
  deriving (Eq, Show)

emptyS :: S
emptyS = S 0 0 (Set.singleton 0)

move :: S -> Move -> S
move s = List.foldl' microMove s . discrete1Moves

discrete1Moves :: Move -> [Dir]
discrete1Moves (M d m) = replicate m d

microMove :: S -> Dir -> S
microMove (S h t@(V2 tx ty) v) m = S nh nt (Set.insert nt v)
  where
  nh@(V2 nhx nhy) =
    h & case m of
      R -> x %~ succ
      L -> x %~ pred
      U -> y %~ succ
      D -> y %~ pred
  nt
    | t `isTouching` nh = t
    | ty == nhy = t & x %~ (dtx +)
    | tx == nhx = t & y %~ (dty +)
    | otherwise = t + dt
  dt@(V2 dtx dty) = diffSign t nh

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
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

-- >>> error . ("This is fine.\n" <>) . draw $ List.foldl' move emptyS example
-- *** Exception: This is fine.
-- ..##.
-- ...##
-- .TH##
-- ....#
-- ####.
draw :: S -> String
draw (S h t v) = unlines [[drawC (V2 px py) | px <- [minX .. maxX]] | py <- reverse [minY .. maxY]]
 where
  drawC p
    | p == h = 'H'
    | p == t = 'T'
    | p `Set.member` v = '#'
    | otherwise = '.'
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
