-- |
-- Module      : Day14
-- Description : Solution to AOC 2021 Day 14: Extended Polymerization
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/14>
module Day14 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Parser

-- | Solution to Day 14.
main14 :: FilePath -> IO ()
main14 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

type P = (Char, Char)
type Gen = (P, P)

type Templates = Map P Gen

parser :: Parser (String, Templates)
parser = (,) <$> (word <* eol <* eol) <*> templates
 where
  templates = Map.fromList . map p2ka <$> pTemp `sepEndBy` eol
  p2ka ((a, b), c) = ((a, b), ((a, c), (c, b)))
  pp :: Parser P
  pp = (,) <$> letterChar <*> letterChar
  pTemp :: Parser (P, Char)
  pTemp = (,) <$> (pp <* space <* string "->" <* space) <*> letterChar

type PCount = Map P Int

-- >>> poly2count "NNCB"
-- fromList [(('C','B'),1),(('N','C'),1),(('N','N'),1)]
poly2count :: String -> PCount
poly2count s = Map.fromListWith (+) $ zip s (drop 1 s) `zip` repeat 1

step :: Templates -> PCount -> PCount
step t = Map.fromListWith (+) . concatMap temp . Map.toList
 where
  temp (p, c) =
    (,c) <$> case Map.lookup p t of
      Nothing -> [p]
      Just (l, r) -> [l, r]

solve :: Int -> (String, Templates) -> Int
solve i (s, t) = max_min mcs
 where
  cs = (!! i) . iterate (step t) $ poly2count s
  mcs = Map.unionsWith (+) [Map.mapKeysWith (+) fst cs, Map.mapKeysWith (+) snd cs]
  max_min x = maximum x `div1` 2 - minimum x `div1` 2
  div1 x m = (x `div` m) + (x `mod` m)

-- >>> solve1 <$> example
-- 1588
solve1 :: (String, Templates) -> Int
solve1 = solve 10

-- >>> solve2 example
-- 2188189693529
solve2 :: (String, Templates) -> Int
solve2 = solve 40

example :: IO (String, Templates)
example =
  parseExample parser . T.unlines $
    [ "NNCB"
    , ""
    , "CH -> B"
    , "HH -> N"
    , "CB -> H"
    , "NH -> C"
    , "HB -> C"
    , "HC -> B"
    , "HN -> C"
    , "NN -> C"
    , "BH -> H"
    , "NC -> B"
    , "NB -> B"
    , "BN -> B"
    , "BB -> N"
    , "BC -> B"
    , "CC -> N"
    , "CN -> C"
    ]