{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Day23
-- Description : Solution to AOC 2020 Day 23: Crab Cups
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/23>
module Day23 where

import Data.Foldable (find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Maybe (fromJust)

-- | Solution to Day 23.
main23 :: FilePath -> IO ()
main23 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

-- Linked list idea
data Circle = C
  { links :: IntMap Int
  , curr :: Int
  , minC :: Int
  , maxC :: Int
  , len :: Int
  }

toList :: Circle -> [Int]
toList c = curr c : toList (c{curr = next c})

next :: Circle -> Int
next c = links c Map.! curr c

parse :: String -> Circle
parse t = C el (head s) (minimum s) (maximum s) (length s)
 where
  s = map (read . (: "")) . head $ lines t
  el = Map.fromList . zip s $ tail s ++ [head s]

rotate :: Circle -> Circle
rotate c = c{curr = next c}

move :: Circle -> Circle
move c = rotate (c{links = Map.fromList up `Map.union` links c})
 where
  rest = tail $ toList c
  (taken, left) = splitAt 3 rest
  makeP n = if n < minC c then maxC c + 1 - minC c + n else n
  dests = map (makeP . (curr c -)) [1 .. 4]
  destE = fromJust $ find (`notElem` taken) dests
  up =
    [ (curr c, head left)
    , (destE, head taken)
    , (last taken, next (c{curr = destE}))
    ]

circleAfter1 :: Circle -> Int
circleAfter1 c = sum . zipWith (*) pows10 . reverse . take (len c - 1) . toList $ rotate (c{curr = 1})

pows10 :: [Int]
pows10 = map (10 ^) [(0 :: Int) ..]

-- >>> solve1 example
-- 67384529
solve1 :: Circle -> Int
solve1 = circleAfter1 . (!! 100) . iterate move

repeatLen :: Int -> Circle -> Circle
repeatLen n c =
  c
    { links = up `Map.union` links c
    , maxC = n
    , len = n
    }
 where
  l = last . take (len c) $ toList c
  up =
    Map.fromList $
      (l, maxC c + 1) :
      (n, curr c) :
      zip [maxC c + 1 .. n - 1] [maxC c + 2 .. n]

twoAfterOne :: Circle -> Int
twoAfterOne c = product . take 2 . toList $ rotate (c{curr = 1})

-- >>> solve2 example
-- 149245887792
solve2 :: Circle -> Int
solve2 = twoAfterOne . (!! 10_000_000) . iterate move . repeatLen 1_000_000

example :: Circle
example = parse "389125467"