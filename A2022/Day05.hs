{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Day05
-- Description : Solution to AOC 2022 Day 05: Supply Stacks
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/05>
module Day05 where

import Data.List qualified as List
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

-- | Solution to Day 05.
main05 :: FilePath -> IO ()
main05 f = do
  input <- parse <$> T.readFile f
  print $ solve1 input
  print $ solve2 input

parse :: Text -> (Stacks, [Procedure])
parse t = case T.splitOn "\n\n" t of
  [ss, ps] ->
    ( parseStacks $ T.unpack ss
    , map parseProcedure . lines $ T.unpack ps
    )
  _ts -> error "Unexpected number of empty line separated parts!"

-- | Stacks of crates ordered left to right, top to bottom.
type Stacks = [[Char]]

parseStacks :: String -> Stacks
parseStacks t = map catMaybes $ List.transpose cs
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
solve1 :: a -> Int
solve1 = errorWithoutStackTrace "Part 1 not implemented"

-- >>> solve2 example
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

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
