{-# LANGUAGE DerivingVia #-}
-- |
-- Module      : Day11
-- Description : Solution to AOC 2021 Day 11: Dumbo Octopus
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/11>
module Day11 where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl', find)
import Data.Map (Map)
import Data.Map qualified as Map
import Linear ( V2(..) )

-- | Solution to Day 11.
main11 :: FilePath -> IO ()
main11 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type P2 = V2 Int

data Energy = Flashed | Energy Int
 deriving (Eq,Ord,Show) 

type Grid = Map P2 Energy

parse :: String -> Grid
parse = Map.fromList . indexYX . map (map (Energy . digitToInt)) . lines
 where
  indexYX :: [[a]] -> [(P2, a)]
  indexYX l = [(V2 x y, a) | (y, ls) <- zip [0 ..] l, (x, a) <- zip [0 ..] ls]

eToInt :: Energy -> Int
eToInt = \case
  Flashed -> 0
  Energy n -> n

inBound :: (P2, P2) -> P2 -> Bool
inBound (V2 lx ly, V2 rx ry) (V2 x y) = lx <= x && x <= rx && ly <= y && y <= ry

adjancent :: Grid -> P2 -> [P2]
adjancent g p = tail . filter (inBound gbounds) . map (p +) $ V2 <$> plusMinus <*> plusMinus
 where
  plusMinus = [0, -1, 1]
  gbounds = (V2 0 0,) . fst $ Map.findMax g

octoStep :: Grid -> P2 -> Grid
octoStep g p = if newFlash then chainG else g'
 where
  (Just e, g') = Map.updateLookupWithKey (const $ Just . octoSucc) p g
  octoSucc = \case
   Flashed -> Flashed 
   Energy n -> if n >= 9 then Flashed else Energy $ succ n 
  chainG = foldl' octoStep g' (adjancent g p)
  newFlash = g Map.! p /= Flashed && e == Flashed

singleStep :: Grid -> Grid
singleStep g = Map.map (Energy . eToInt) $ foldl' octoStep g (Map.keys g)

stepCount :: (Int, Grid) -> (Int, Grid)
stepCount (c, g) = (c + fc, sg)
 where
  sg = singleStep g
  fc = count (Energy 0) (Map.elems sg)
  count e = length . filter (== e)

-- >>> solve1 example
-- 1656
solve1 :: Grid -> Int
solve1 = fst . (!! 100) . iterate stepCount . (0,)

indexedSteps :: Grid -> [(Int, Grid)]
indexedSteps = zip [0..] . iterate singleStep

-- >>> solve2 example
-- 195
solve2 :: Grid -> Int
solve2 = maybe 0 fst . find (all (==Energy 0) . snd) . indexedSteps

prettyGrid :: Grid -> IO ()
prettyGrid g = mapM_ putStrLn cgrid
 where
  (V2 mx my) = fst $ Map.findMax g
  cgrid = [[intToDigit . eToInt $ g Map.! V2 x y | x <- [0 .. mx]] | y <- [0 .. my]]

prettySteps :: Int -> Grid -> IO ()
prettySteps i = mapM_ prettyStep . take i . indexedSteps
  where
    prettyStep (j,g) = print j >> prettyGrid g

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
