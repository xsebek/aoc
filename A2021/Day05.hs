-- |
-- Module      : Day05
-- Description : Solution to AOC 2021 Day 05: Hydrothermal Venture
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/05>
module Day05 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Parser

-- | Solution to Day 05.
main05 :: FilePath -> IO ()
main05 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input
  -- prettyCount . pointCounter diagonalToo $ input

data V2 = V2 {x :: Int, y :: Int} deriving (Eq, Ord, Show)

type Line = (V2, V2)

parser :: Parser [Line]
parser = parserLine `endBy` eol
 where
  parserLine :: Parser Line
  parserLine = (,) <$> parserV2 <*> (space *> string "->" *> space *> parserV2)
  parserV2 :: Parser V2
  parserV2 = V2 <$> decimal <*> (char ',' *> decimal)

straightLine :: Line -> Maybe [V2]
straightLine (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = Just [V2 x1 y | y <- fromTo y1 y2]
  | y1 == y2 = Just [V2 x y1 | x <- fromTo x1 x2]
  | otherwise = Nothing
  where
    fromTo i j = [min i j..max i j]

count :: Ord a => [a] -> Map a Int
count = foldr (flip (Map.insertWith (+)) 1) mempty

pointCounter :: (Line -> Maybe [V2]) -> [Line] -> Map V2 Int
pointCounter lToMV2 = count . concat . mapMaybe lToMV2

-- >>> solve1 <$> exampleI
-- 5
solve1 :: [Line] -> Int
solve1 = length . Map.filter (2<=) . pointCounter straightLine

-- >>> diagonalToo (V2 0 2, V2 2 0)
diagonalToo :: Line -> Maybe [V2]
diagonalToo l@(V2 x1 y1, V2 x2 y2)
 | isJust sl = sl 
 | abs dx == abs dy = Just [V2 (x2 + sx*i) (y2 + sy*i) | i <- [0..abs dx]]
 | otherwise = Nothing
  where
    sl = straightLine l
    (dx, dy) = (x1 - x2, y1 - y2)
    (sx, sy) = (signum dx, signum dy)

-- >>> solve2 <$> exampleI
-- 12
solve2 :: [Line] -> Int
solve2 = length . Map.filter (2<=) . pointCounter diagonalToo

prettyLine :: Line -> String
prettyLine (v1, v2) =
  let pr2 (V2 x y) = show x <> "," <> show y
   in pr2 v1 <> " -> " <> pr2 v2

prettyLines :: [Line] -> IO ()
prettyLines = mapM_ (putStrLn . prettyLine)

-- >>> exampleI >>= prettyCount . pointCounter diagonalToo
-- 1.1....11.
-- .111...2..
-- ..2.1.111.
-- ...1.2.2..
-- .112313211
-- ...1.2....
-- ..1...1...
-- .1.....1..
-- 1.......1.
-- 222111....
prettyCount :: Map V2 Int -> IO ()
prettyCount m = mapM_ putStrLn cmap 
  where
    maxX = maximum . map x $ Map.keys m
    maxY = maximum . map y $ Map.keys m
    cmap = [concat [ maybe "." show $ m Map.!? V2 x y | x <- [0..maxX]] | y <- [0..maxY]]


-- >>> exampleI >>= prettyLines . take 2
-- 0,9 -> 5,9
-- 8,0 -> 0,8
exampleI :: IO [Line]
exampleI = parseExample parser example

example :: Text
example =
  T.unlines
    [ "0,9 -> 5,9"
    , "8,0 -> 0,8"
    , "9,4 -> 3,4"
    , "2,2 -> 2,1"
    , "7,0 -> 7,4"
    , "6,4 -> 2,0"
    , "0,9 -> 2,9"
    , "3,4 -> 1,4"
    , "0,0 -> 8,8"
    , "5,5 -> 8,2"
    ]
