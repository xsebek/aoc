{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Day23
-- Description : Solution to AOC 2021 Day 23: Amphipod
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/23>
module Day23 where

import Algorithm.Search (dijkstraAssoc)
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Linear
import Optics

-- | Solution to Day 23.
main23 :: FilePath -> IO ()
main23 f = do
  input <- parse <$> readFile f
  --mapM_ (\i -> print i >> (case solveCheapest i example of Just (c,bs) -> print c >> mapM_ pretty bs; _ -> putStrLn "Nothing")) [1..16]
  mapM_ (\i -> print i >> (case solveCheapest i input of Just (c, bs) -> print c >> mapM_ pretty bs; _ -> putStrLn "Nothing")) [15 .. 20]
  print $ solve1 input
  print $ solve2 input

type P2 = V2 Int

data Amphipod = A | B | C | D
  deriving (Eq, Ord, Enum, Show, Read)

data SpaceF a
  = Wall
  | Hall a
  | Space a
  deriving (Eq, Ord, Functor, Show)

type Space = SpaceF (Maybe Amphipod)

type Burrow = Map P2 Space

parse :: String -> Burrow
parse s = Map.fromList l
 where
  l = [(V2 x y, parseSpace c) | (y, ls) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] ls, not (isSpace c)]
  parseSpace c
    | c == '#' = Wall
    | c == '.' = Hall Nothing
    | c == '_' = Space Nothing
    | otherwise = Space (Just $ read [c])

type Path = [P2]

neighbors :: P2 -> [P2]
neighbors (V2 x y) = [V2 (x + i) (y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], abs (i + j) == 1]

empty :: Space -> Bool
empty = \case
  Hall Nothing -> True
  Space Nothing -> True
  _ -> False

paths :: Burrow -> Set P2 -> P2 -> [Path]
paths b s p = map (p :) ps
 where
  s' = Set.insert p s
  ns = filter (`Set.notMember` s) $ neighbors p
  pos = map fst . filter (empty . snd) $ zip ns (map (b Map.!) ns)
  ps = map (: []) pos <> concatMap (paths b s') pos

amphiPaths :: Burrow -> [(Amphipod, Path)]
amphiPaths b = splits $ hpaths' <> bpaths
 where
  pods f = mapMaybe f $ Map.toList b
  hpods = pods (\case (p, Hall (Just a)) -> Just (a, p); _ -> Nothing)
  bpods = pods (\case (p, Space (Just a)) -> Just (a, p); _ -> Nothing)
  filterL c = filter (c . last)
  podPaths cond = map $ second (filterL (not . aboveBurrow) . filterL cond . paths b mempty)
  aboveBurrow p = isSHall' p && isSSpace' (p + V2 0 1)
  hpaths = podPaths ((== Space Nothing) . (b Map.!)) hpods
  hpaths' = map (\(a, ps) -> (a,) $ filter (\p -> not (pathFromDone b (a, p)) && organized Map.! last p == Space (Just a)) ps) hpaths
  bpaths = podPaths ((== Hall Nothing) . (b Map.!)) bpods
  splits = concatMap (\(a, bs) -> map (a,) bs)

amphiCost :: (Amphipod, Path) -> Int
amphiCost (a, p) = cost a * (length p - 1)

cost :: Amphipod -> Int
cost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

move :: (Amphipod, Path) -> Burrow -> Burrow
move (a, p) b =
  b
    & Map.update (swaps $ Just a) (last p)
    & Map.update (swaps Nothing) (head p)
 where
  swaps :: Maybe Amphipod -> Space -> Maybe Space
  swaps m = Just . fmap (const m)

solveCheapest :: Int -> Burrow -> Maybe (Int, [Burrow])
solveCheapest steps =
  fmap (second $ map snd)
    . dijkstraAssoc
      assocStep
      -- (getMinimum . snd)
      ((== organized) . snd)
    . (0,)
 where
  assocStep :: (Int, Burrow) -> [((Int, Burrow), Int)]
  assocStep (n, b) = if n > steps then [] else map (\ap -> ((n + 1, move ap b), amphiCost ap)) $ amphiPaths b

pathFromDone :: Burrow -> (Amphipod, Path) -> Bool
pathFromDone b (_a, ps) = startsInGood
 where
  start = head ps
  lower = start + V2 0 1
  same p = b Map.! p == organized Map.! p
  startsInGood = same start && same lower

halls :: Set P2
halls = Map.keysSet $ Map.filter isSHall emptyBurrow

spaces :: Set P2
spaces = Map.keysSet $ Map.filter isSSpace emptyBurrow

isSHall :: Space -> Bool
isSHall = \case Hall _ -> True; _ -> False

isSSpace :: Space -> Bool
isSSpace = \case Space _ -> True; _ -> False

isSHall' :: P2 -> Bool
isSHall' p = Set.member p halls

isSSpace' :: P2 -> Bool
isSSpace' p = Set.member p spaces

home :: Amphipod -> [P2]
home a = Map.keys $ Map.filter (== Space (Just a)) organized

homes :: [[P2]]
homes = map home [A .. D]

pathHome :: (P2, Amphipod) -> Int
pathHome (p, a) = (cost a *) . pred . minimum $ map (sum . fmap abs . subtract p) (homes !! fromEnum a)

getMinimum :: Burrow -> Int
getMinimum b = sum $ map pathHome aps
 where
  s2ma :: Space -> Maybe Amphipod
  s2ma = \case
    Wall -> Nothing
    Hall ma -> ma
    Space ma -> ma
  aps = Map.toList $ Map.mapMaybe s2ma b

-- >>> solve1 example
solve1 :: a -> Int
solve1 = errorWithoutStackTrace "Part 1 not implemented"

-- >>> solve2 example
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

prettySpace :: Space -> Char
prettySpace = \case
  Wall -> '#'
  Hall m_am -> maybe '.' (head . show) m_am
  Space m_am -> maybe '_' (head . show) m_am

pretty :: Burrow -> IO ()
pretty b =
  mapM_
    putStrLn
    [ [ maybe ' ' prettySpace ms
      | x <- [0 .. maximumD _x]
      , let ms = Map.lookup (V2 x y) b
      ]
    | y <- [0 .. maximumD _y]
    ]
 where
  maximumD :: LensVL' P2 Int -> Int
  maximumD d = maximum . map (view $ lensVL d) $ Map.keys b

prettyPaths :: Burrow -> IO ()
prettyPaths b = mapM_ (\ap@(a, p) -> print a >> print p >> print (amphiCost ap) >> pretty (move ap b)) $ amphiPaths b

emptyBurrow :: Burrow
emptyBurrow =
  parse . unlines $
    [ "#############"
    , "#...........#"
    , "###_#_#_#_###"
    , "  #_#_#_#_#"
    , "  #########"
    ]

organized :: Burrow
organized =
  parse . unlines $
    [ "#############"
    , "#...........#"
    , "###A#B#C#D###"
    , "  #A#B#C#D#"
    , "  #########"
    ]

example :: Burrow
example =
  parse . unlines $
    [ "#############"
    , "#...........#"
    , "###B#C#B#D###"
    , "  #A#D#C#A#"
    , "  #########"
    ]
