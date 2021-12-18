-- |
-- Module      : Day17
-- Description : Solution to AOC 2021 Day 17: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/17>
module Day17 where

import Linear

-- | Solution to Day 17.
main17 :: FilePath -> IO ()
main17 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type P2 = V2 Integer

-- >>> parse "target area: x=124..174, y=-123..-86"
parse :: String -> (P2, P2)
parse = const (V2 124 (-123), V2 174 (-86))

step :: (P2, P2) -> (P2, P2)
step (speed@(V2 x y), pos) = (V2 (max 0 $ x - 1) (y - 1), pos + speed)

-- >>> solve1 example
solve1 :: (P2, P2) -> Integer
solve1 (V2 _ y1, V2 _ y2) = sum [0 .. - (max y1 y2) -1]

inside :: P2 -> (P2, P2) -> Bool
inside (V2 x y) (V2 x1 y1, V2 x2 y2) = inRange x (x1,x2) && inRange y (y1,y2)
 where
  inRange i (l,r) = min l r <= i && i <= max l r

isGoodShot :: (P2, P2) -> P2 -> Bool
isGoodShot rect p = any (`inside` rect) . take 1000 . map snd $ iterate step (p, V2 0 0)

-- >>> solve2 example
solve2 :: (P2, P2) -> Int
solve2 rect@(V2 x1 y1, V2 x2 y2) = length . filter (isGoodShot rect) $ [V2 x y | x <- [0 .. max x1 x2 + 10], y <- [min y2 y1.. negate $ min y1 y2]]

-- >>> parse "target area: x=20..30, y=-10..-5"
example :: (P2, P2)
example = (V2 20 (-10), V2 30 (-5))


