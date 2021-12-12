-- |
-- Module      : Day12
-- Description : Solution to AOC 2021 Day 12: Passage Pathing
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/12>
module Day12 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Char (isLower)

-- | Solution to Day 12.
main12 :: FilePath -> IO ()
main12 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type Cave = String

type CaveSystem = Map Cave (Set Cave)

parse :: String -> CaveSystem
parse = edgesList . map lineToEdge . lines
 where
  lineToEdge :: String -> (Cave, Cave)
  lineToEdge = fmap (drop 1) . break (== '-')
  edgesList :: [(Cave,Cave)] -> CaveSystem
  edgesList = Map.fromListWith Set.union . map (Set.singleton <$>) . concatMap undirectedEdge
  undirectedEdge :: (x,x) -> [(x,x)]
  undirectedEdge (x,y) = [(x,y), (y,x)]

type Visited = Set Cave

pathsFromTo :: Bool -> Visited -> Cave -> Cave -> CaveSystem -> [[Cave]]
pathsFromTo twice v s e cs = endP paths
 where
  nV = if all isLower s then Set.insert s v else v
  adj = filter (\c -> c /= "start" && (twice || c `Set.notMember` nV)) . Set.toList $ cs Map.! s
  endP = if e `elem` adj then ([e]:) else id
  paths = concatMap (\c -> pathsFromTo (twice && c `Set.notMember` v) nV c e cs) $ filter (/=e) adj

-- >>> solve1 example
-- 10
solve1 :: CaveSystem -> Int
solve1 = length . pathsFromTo False mempty "start" "end"

-- >>> solve2 example
-- 36
solve2 :: CaveSystem -> Int
solve2 = length . pathsFromTo True mempty "start" "end"

example :: CaveSystem
example = parse . unlines $
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

exampleLarge :: CaveSystem
exampleLarge = parse . unlines $
  [ "dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"
  ]

exampleLarger :: CaveSystem
exampleLarger = parse . unlines $
  [ "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]