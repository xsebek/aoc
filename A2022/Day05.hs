{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- |
-- Module      : Day05
-- Description : Solution to AOC 2022 Day 05: Supply Stacks
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/05>
module Day05 where

import Data.List qualified as List
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import qualified Data.IntMap.Lazy as M
import Data.IntMap (IntMap)

-- | Solution to Day 05.
main05 :: FilePath -> IO ()
main05 f = do
  input <- parse <$> T.readFile f
  putStrLn $ solve1 input
  putStrLn $ solve2 input

parse :: Text -> (Stacks, [Procedure])
parse t = case T.splitOn "\n\n" t of
  [ss, ps] ->
    ( parseStacks $ T.unpack ss
    , map parseProcedure . lines $ T.unpack ps
    )
  _ts -> error "Unexpected number of empty line separated parts!"

-- | Stacks of crates ordered left to right, top to bottom.
type Stacks = IntMap [Char]

parseStacks :: String -> Stacks
parseStacks t = M.fromList . zip [1..] . map catMaybes $ List.transpose cs
 where
  cs = map (map s2c . chunksOf 4) rl
  rl = init $ lines t
  s2c s = case take 3 s of
    ['[', c, ']'] -> Just c
    _otherwise -> Nothing

data Procedure = Move
  { quantity :: Int
  , from :: Int
  , to :: Int
  }
  deriving (Eq, Show)

parseProcedure :: String -> Procedure
parseProcedure p = case words p of
  ["move", q, "from", f, "to", t] -> Move (read q) (read f) (read t)
  other -> error $ "Could not parse procedure: " <> show other

-- >>> solve1 example
-- "CMZ"
solve1 :: (Stacks, [Procedure]) -> String
solve1 (s,ps) = map head . M.elems $ List.foldl' (flip move) s ps

-- >>> move (Move 1 2 1) (fst example)
-- fromList [(1,"DNZ"),(2,"CM"),(3,"P")]
move :: Procedure -> Stacks -> Stacks
move = moveG reverse

moveG :: (String -> String) -> Procedure -> Stacks -> Stacks
moveG f p s = rs
  where
    (a, ds) = M.alterF (fmap Just . List.splitAt p.quantity . fromJust) p.from s
    rs = M.adjust (f a <>) p.to ds

-- >>> solve2 example
-- "MCD"
solve2 :: (Stacks, [Procedure]) -> String
solve2 (s,ps) = map head . M.elems $ List.foldl' (flip $ moveG id) s ps

-- >>> fst example
-- fromList [(1,"NZ"),(2,"DCM"),(3,"P")]
-- >>> head $ snd example
-- Move {quantity = 1, from = 2, to = 1}
example :: (Stacks, [Procedure])
example =
  parse . T.unlines $
    [ "    [D]    "
    , "[N] [C]    "
    , "[Z] [M] [P]"
    , " 1   2   3 "
    , ""
    , "move 1 from 2 to 1"
    , "move 3 from 1 to 3"
    , "move 2 from 2 to 1"
    , "move 1 from 1 to 2"
    ]
