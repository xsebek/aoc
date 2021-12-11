{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : Day11
-- Description : Solution to AOC 2021 Day 11: Dumbo Octopus
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/11>
module Day11 where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (find, foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..))

-- | Solution to Day 11.
main11 :: FilePath -> IO ()
main11 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type P2 = V2 Int

type Grid = Map P2 Int

parse :: String -> Grid
parse = Map.fromList . indexYX . map (map digitToInt) . lines
 where
  indexYX :: [[a]] -> [(P2, a)]
  indexYX l = [(V2 x y, a) | (y, ls) <- zip [0 ..] l, (x, a) <- zip [0 ..] ls]

inBound :: (P2, P2) -> P2 -> Bool
inBound (V2 lx ly, V2 rx ry) (V2 x y) = lx <= x && x <= rx && ly <= y && y <= ry

adjancent :: Grid -> P2 -> [P2]
adjancent g p = tail . filter (inBound gbounds) . map (p +) $ V2 <$> plusMinus <*> plusMinus
 where
  plusMinus = [0, -1, 1]
  gbounds = (V2 0 0,) . fst $ Map.findMax g

octoStep :: (Set P2, Grid) -> P2 -> (Set P2, Grid)
octoStep (flashed, g) p = if newFlash then chainG else (flashed, g')
 where
  (Just e, g') = Map.updateLookupWithKey (\k -> Just . octoSucc k) p g
  octoSucc k n
    | n >= 9 = 0
    | n == 0 = if Set.member k flashed then 0 else 1
    | otherwise = succ n
  chainG = foldl' octoStep (Set.insert p flashed, g') (adjancent g p)
  newFlash = Set.notMember p flashed && e == 0

singleStep :: Grid -> Grid
singleStep g = snd $ foldl' octoStep (mempty, g) (Map.keys g)

stepCount :: (Int, Grid) -> (Int, Grid)
stepCount (c, g) = (c + fc, sg)
 where
  sg = singleStep g
  fc = count 0 (Map.elems sg)
  count e = length . filter (== e)

-- >>> solve1 example
-- 1656
solve1 :: Grid -> Int
solve1 = fst . (!! 100) . iterate stepCount . (0,)

indexedSteps :: Grid -> [(Int, Grid)]
indexedSteps = zip [0 ..] . iterate singleStep

-- >>> solve2 example
-- 195
solve2 :: Grid -> Int
solve2 = maybe 0 fst . find (all (== 0) . snd) . indexedSteps

prettyGrid :: Grid -> IO ()
prettyGrid g = mapM_ putStrLn cgrid
 where
  (V2 mx my) = fst $ Map.findMax g
  cgrid = [[intToDigit $ g Map.! V2 x y | x <- [0 .. mx]] | y <- [0 .. my]]

prettySteps :: Int -> Grid -> IO ()
prettySteps i = mapM_ prettyStep . take i . indexedSteps
 where
  prettyStep (j, g) = print j >> prettyGrid g

smallExample :: Grid
smallExample =
  parse . unlines $
    [ "11111"
    , "19991"
    , "19191"
    , "19991"
    , "11111"
    ]

example :: Grid
example =
  parse . unlines $
    [ "5483143223"
    , "2745854711"
    , "5264556173"
    , "6141336146"
    , "6357385478"
    , "4167524645"
    , "2176841721"
    , "6882881134"
    , "4846848554"
    , "5283751526"
    ]
