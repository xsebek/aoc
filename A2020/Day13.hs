-- |
-- Module      : Day13
-- Description : Solution to AOC 2020 Day 13: Shuttle Search
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/13>
module Day13 where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Foldable (foldl', minimumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Text.Read (readEither)

newtype Bus = Bus {unbus :: Maybe Int} deriving (Eq, Ord)

data Notes = Notes {depart :: Int, buses :: [Bus]} deriving (Eq, Show)

-- | Solution to Day 13.
main13 :: FilePath -> IO ()
main13 f = do
  input <- parse =<< readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "13"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

-- >>> parse example
-- Notes {depart = 939, buses = [7,13,x,x,59,x,31,19]}
parse :: MonadFail m => String -> m Notes
parse = either fail pure . toNotes . lines
  where
    toNotes = \case
      [dep, bs] -> Notes <$> readEither dep <*> toNums bs
      _ -> Left "Wrong number of lines!"
    toNums = mapM toBus . words . map (\c -> if c == ',' then ' ' else c)
    toBus x = Bus <$> if x == "x" then pure Nothing else Just <$> readEither x

-- >>> solve1 <$> parse example
-- 295
solve1 :: Notes -> Int
solve1 = uncurry (*) . closestBus

-- >>> closestBus <$> parse example
-- (59,5)
closestBus :: Notes -> (Int, Int)
closestBus n = shortest $ zip bs waits
  where
    shortest = minimumBy (compare `on` snd)
    waits = map (waitTime $ depart n) bs
    bs = mapMaybe unbus (buses n)

-- >>> waitTime 939 59
-- 5
waitTime :: Int -> Int -> Int
waitTime dep bus = negate (dep `mod` bus) `mod` bus

-- >>> solve2 <$> parse example
-- 1068781
solve2 :: Notes -> Int
solve2 = chinese . minutePauses . buses

-- >>> minutePauses . buses <$> parse example
-- [(0,7),(-1,13),(-4,59),(-6,31),(-7,19)]
minutePauses :: [Bus] -> [(Int, Int)]
minutePauses = keepBuses . zip [0, -1 ..] . map unbus
  where
    keepBuses :: [(a, Maybe b)] -> [(a, b)]
    keepBuses = mapMaybe $ uncurry (fmap . (,))

example :: String
example = "939\n7,13,x,x,59,x,31,19"

instance Show Bus where
  show (Bus b) = maybe "x" show b

chinese :: [(Int, Int)] -> Int
chinese l = (`mod` nAll) . foldl' (+) 0 $ map go l
  where
    nAll = foldl' (*) 1 $ map snd l
    go (ai, ni) = ai * mi * nI
      where
        nI = nAll `div` ni
        (_, mi) = besout ni nI

besout :: Int -> Int -> (Int, Int)
besout _ 0 = (1, 0)
besout a b = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = besout b r
