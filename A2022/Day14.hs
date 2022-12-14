-- |
-- Module      : Day14
-- Description : Solution to AOC 2022 Day 14: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/14>
module Day14 where

import Data.Bifunctor (bimap)
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Linear (V2 (..))

-- | Solution to Day 14.
main14 :: FilePath -> IO ()
main14 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> Cave
parse = caveFromLines . map parseLine . lines

caveFromLines :: [Set P] -> Cave
caveFromLines = M.fromSet (const Rock) . S.unions

data Entity = Sand | Rock
  deriving (Eq, Show)

type Cave = M.Map P Entity

type P = V2 Int

getX :: V2 a -> a
getX (V2 x _y) = x

getY :: V2 a -> a
getY (V2 _x y) = y

parseLine :: String -> Set P
parseLine = S.fromList . toPS . map parsePoint . filter (/= "->") . words
 where
  parsePoint = bimap read (read . drop 1) . break (== ',')
  toPS (p1@(x1, y1) : p2@(x2, y2) : ps)
    | x1 == x2 = [V2 x1 y | y <- y1 `to` y2] <> toPS (p2 : ps)
    | y1 == y2 = [V2 x y1 | x <- x1 `to` x2] <> toPS (p2 : ps)
    | otherwise = error $ "not vertical/horizontal line:" <> show (p1, p2)
  toPS _ = []
  to i j = [min i j .. max i j]

-- >>> solve1 example
-- 24
solve1 :: Cave -> Int
solve1 c = length $ fallingSand ((maxY >) . getY) c
 where
  maxY = maximum $ map getY $ M.keys c

start :: P
start = V2 500 0

fallingSand :: (P -> Bool) -> Cave -> [Cave]
fallingSand check c = case fallWhile check start c of
  Nothing -> []
  Just p ->
    let nc = M.insert p Sand c
     in if p == start then [nc] else nc : fallingSand check nc

-- Only when the sand resting position does not match predicate
-- return Nothing.
--
-- >>> fallWhile (const True) start example
-- Just (V2 500 8)
fallWhile :: (P -> Bool) -> P -> Cave -> Maybe P
fallWhile check p c = case fall1 p c of
  Nothing -> pure p
  Just np -> if check np then fallWhile check np c else Nothing

-- If the sand can NOT fall, return Nothing
fall1 :: P -> Cave -> Maybe P
fall1 p c = List.find freeP below
 where
  below = map ((+ p) . flip V2 1) [0, -1, 1]
  freeP = flip M.notMember c

-- >>> solve2 example
-- 93
solve2 :: Cave -> Int
solve2 = length . fallingSand (const True) . addFloor

-- >>> error . ("This is fine\n" <>) $ prettyCave $ addFloor example

-- *** Exception: This is fine

-- ...........+...........
-- .......................
-- .......................
-- .......................
-- .........#...##........
-- .........#...#.........
-- .......###...#.........
-- .............#.........
-- .............#.........
-- .....#########.........
-- .......................
-- #######################
addFloor :: Cave -> Cave
addFloor c = M.union c bottom
 where
  bottom = M.fromList (map (\x -> (V2 x maxY, Rock)) [minX .. maxX])
  minX = getX start - maxY
  maxX = getX start + maxY
  maxY = (+ 2) . maximum $ map getY $ M.keys c

-- >>> error . ("This is fine\n" <>) $ prettyCave example

-- *** Exception: This is fine

-- ......+...
-- ..........
-- ..........
-- ..........
-- ....#...##
-- ....#...#.
-- ..###...#.
-- ........#.
-- ........#.
-- #########.
prettyCave :: Cave -> String
prettyCave cave =
  unlines
    [ [ -- if p == start
      -- then '+'
      -- else
      fc $ M.lookup p cave
      | x <- [minX .. maxX]
      , let p = V2 x y
      ]
    | y <- [0 .. maxY]
    ]
 where
  fc Nothing = '.'
  fc (Just Rock) = '#'
  fc (Just Sand) = 'o'
  xs = map getX $ M.keys cave
  ys = map getY $ M.keys cave
  minX = minimum xs
  maxX = maximum xs
  maxY = maximum ys

-- >>> example
example :: Cave
example =
  parse . unlines $
    [ "498,4 -> 498,6 -> 496,6"
    , "503,4 -> 502,4 -> 502,9 -> 494,9"
    ]
