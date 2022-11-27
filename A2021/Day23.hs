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
import Data.Maybe (isNothing, mapMaybe, maybeToList, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Linear
import Optics
import Text.Read (readMaybe)

-- | Solution to Day 23.
main23 :: FilePath -> IO ()
main23 f = do
  inputs <- readFile f
  -- let input1 = parse inputs
  -- let solve input = mapM_ (\i -> print i >> (case solveCheapest i input of Just (c, bs) -> print c >> mapM_ pretty bs; _ -> putStrLn "Nothing"))
  -- solve input1 [15,14..0]
  -- let input2 = let l = lines inputs in parse . unlines $ take 3 l <> hiddenLines <> drop 3 l
  -- solve (input2{organized = organized2}) [35]
  print $ solve1 inputs
  print $ solve2 inputs

type P2 = V2 Int

data Amphipod = A | B | C | D
  deriving (Eq, Ord, Enum, Show, Read)

data Space
  = Wall
  | Hall
  | Space
  deriving (Eq, Ord, Show)

type Pods = Map P2 Amphipod

data Burrows = Burrows
  { burrow :: Map P2 Space
  , pods :: Map P2 Amphipod
  , organized :: Map P2 Amphipod
  , isSSpace :: P2 -> Bool
  , isSHall :: P2 -> Bool
  , maxX :: Int
  , maxY :: Int
  }

parse :: String -> Burrows
parse s =
  Burrows
    { burrow = b
    , pods = ps
    , organized = organized1
    , isSSpace = (`Set.member` spaces)
    , isSHall = (`Set.member` halls)
    , maxX = maximumD _x
    , maxY = maximumD _y
    }
 where
  ps = Map.fromList [(V2 x y, p) | (y, ls) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] ls, p <- maybeToList (readMaybe [c])]
  b = Map.fromList [(V2 x y, parseSpace c) | (y, ls) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] ls, not (isSpace c)]
  halls = Map.keysSet $ Map.filter (== Hall) b
  spaces = Map.keysSet $ Map.filter (== Space) b
  parseSpace = \case
    '#' -> Wall
    '.' -> Hall
    _ -> Space
  maximumD :: LensVL' P2 Int -> Int
  maximumD d = maximum . map (view $ lensVL d) $ Map.keys b

type Path = [P2]

neighbors :: P2 -> [P2]
neighbors (V2 x y) = [V2 (x + i) (y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], abs (i + j) == 1]

paths :: Burrows -> Set P2 -> P2 -> [Path]
paths b@Burrows{..} s p = map (p :) ps
 where
  s' = Set.insert p s
  pos = filter (\p2 -> empty p2 && Set.notMember p2 s && Map.notMember p2 pods) $ neighbors p
  ps = map (: []) pos <> concatMap (paths b s') pos
  empty p2 = isSSpace p2 || isSHall p2

amphiPaths :: Burrows -> [(Amphipod, Path)]
amphiPaths b@Burrows{..} = splits $ hpaths' <> bpaths
 where
  lpods f = mapMaybe f $ Map.toList pods
  hpods = lpods (\(p, a) -> if isSHall p then Just (a, p) else Nothing) -- (\case (p, Hall (Just a)) -> Just (a, p); _ -> Nothing)
  bpods = lpods (\(p, a) -> if isSSpace p then Just (a, p) else Nothing) -- (\case (p, Space (Just a)) -> Just (a, p); _ -> Nothing)
  filterL c = filter (c . last)
  podPaths cond = map $ second (filterL (not . aboveBurrow) . filterL cond . paths b mempty)
  aboveBurrow p = isSHall p && isSSpace (p + V2 0 1)
  bpaths = podPaths isSHall bpods
  hpaths = podPaths isSSpace hpods
  hpaths' = map (\(a, ps) -> (a,) $ filter (\p -> let ap = (a, p) in notPathFromDone b ap && pathToHome b ap) ps) hpaths
  splits = concatMap (\(a, bs) -> map (a,) bs)

notPathFromDone :: Burrows -> (Amphipod, Path) -> Bool
notPathFromDone Burrows{..} (_a, ps) =
  let (V2 x _) = last ps
      slope = [V2 x y | y <- [2 .. maxY - 1]]
      same p = pods Map.!? p  == organized Map.!? p
   in not $ all same slope

pathToHome :: Burrows -> (Amphipod, Path) -> Bool
pathToHome b ap = go (move ap b) (last $ snd ap)
 where
  go Burrows{..} (V2 x _) =
    let slope = [V2 x y | y <- [2 .. maxY - 1]]
        same p = let sp = pods Map.!? p in isNothing sp || sp == organized Map.!? p
     in all same slope

amphiCost :: (Amphipod, Path) -> Int
amphiCost (a, p) = cost a * (length p - 1)

cost :: Amphipod -> Int
cost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

move :: (Amphipod, Path) -> Burrows -> Burrows
move (a, p) b =
  b
    { pods =
        pods b
          & Map.insert (last p) a
          & Map.delete (head p)
    }

solveCheapest :: Int -> Burrows -> Maybe (Int, [Burrows])
solveCheapest steps burs =
  second (map (setp . snd))
    <$> dijkstraAssoc
      assocStep
      ((== organized burs) . snd)
      (1, pods burs)
 where
  setp p = burs{pods = p}
  assocStep :: (Int, Pods) -> [((Int, Pods), Int)]
  assocStep (n, p) = if n > steps then [] else map (\ap -> ((n + 1, pods $ move ap (setp p)), amphiCost ap)) $ amphiPaths (setp p)

solveCheapest' :: Burrows -> (Int, [Burrows])
solveCheapest' b = fromMaybe (error "Impossible to solve!") $ solveCheapest (2 * length (pods b)) b

-- >>> solve1 example
solve1 :: String -> Int
solve1 = fst . solveCheapest' . parse

hiddenLines :: [String]
hiddenLines =
  [ "  #D#C#B#A#"
  , "  #D#B#A#C#"
  ]

-- >>> solve2 example
solve2 :: String -> Int
solve2 input = fst $ solveCheapest' (hidden{organized = organized2})
  where
    l = lines input
    hidden = parse . unlines $ take 3 l <> hiddenLines <> drop 3 l

prettySpace :: Space -> Char
prettySpace = \case
  Wall -> '#'
  Hall -> '.'
  Space -> '_'

pretty :: Burrows -> IO ()
pretty Burrows{..} =
  mapM_
    putStrLn
    [ [ if Map.member p pods then head . show $ pods Map.! p else maybe ' ' prettySpace ms
      | x <- [0 .. maximumD _x]
      , let p = V2 x y
      , let ms = Map.lookup p burrow
      ]
    | y <- [0 .. maximumD _y]
    ]
 where
  maximumD :: LensVL' P2 Int -> Int
  maximumD d = maximum . map (view $ lensVL d) $ Map.keys burrow

prettyPaths :: Burrows -> IO ()
prettyPaths b = mapM_ (\ap@(a, p) -> print a >> print p >> print (amphiCost ap) >> pretty (move ap b)) $ amphiPaths b

organized1 :: Pods
organized1 =
  pods . parse . unlines $
    [ "#############"
    , "#...........#"
    , "###A#B#C#D###"
    , "  #A#B#C#D#"
    , "  #########"
    ]

organized2 :: Pods
organized2 =
  pods . parse . unlines $
    [ "#############"
    , "#...........#"
    , "###A#B#C#D###"
    , "  #A#B#C#D#"
    , "  #A#B#C#D#"
    , "  #A#B#C#D#"
    , "  #########"
    ]

example :: Burrows
example =
  parse . unlines $
    [ "#############"
    , "#...........#"
    , "###B#C#B#D###"
    , "  #A#D#C#A#"
    , "  #########"
    ]
