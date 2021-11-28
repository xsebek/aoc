-- |
-- Module      : Day24
-- Description : Solution to AOC 2020 Day 24: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/24>
module Day24 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Identity (Identity(runIdentity, Identity))


-- | Solution to Day 24.
main24 :: FilePath -> IO ()
main24 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [[HDir]]
parse = map parseHDirs . lines

-- e, se, sw, w, nw, and ne

data HDir = E | SE | SW | W | NW | NE
  deriving (Eq, Ord, Enum, Bounded, Show)

parseHDirs :: String -> [HDir]
parseHDirs = \case
  [] -> []
  's' : 'e' : s -> SE : parseHDirs s
  's' : 'w' : s -> SW : parseHDirs s
  'n' : 'w' : s -> NW : parseHDirs s
  'n' : 'e' : s -> NE : parseHDirs s
  'e' : s -> E : parseHDirs s
  'w' : s -> W : parseHDirs s
  d : _ -> error $ "unrecognized direction: " <> show d

-- | Hexagonal grid position as 3D vector.
--
-- https://www.redblobgames.com/grids/hexagons/#coordinates-cube

{-
       / \     / \
     /     \ /     \
    |0,-1,+1|+1,-1,0|
    |   NW  |   NE  |
   / \     / \     / \
 /     \ /     \ /     \
|-1,0,+1| 0,0,0 |+1,0,-1|
|   W   |  REF  |   E   |
 \     / \     / \     /
   \ /     \ /     \ /
    |-1,+1,0|0,+1,-1|
    |  SW   |   SE  |
     \     / \     /
       \ /     \ /

-}
data HVec = H
  { q :: Int
  , r :: Int
  , s :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup HVec where
  (H s1 q1 r1) <> (H s2 q2 r2) = H (s1 + s2) (q1 + q2) (r1 + r2)

instance Monoid HVec where
  mempty = H 0 0 0

-- >>> all (== mempty) [ move x (move y mempty) | (x,y) <- [(W,E), (SE,NW), (SW,NE)]]
-- True
move :: HDir -> HVec -> HVec
move d = (<>) $ case d of
  E -> H 1 0 (-1)
  SE -> H 0 1 (-1)
  SW -> H (-1) 1 0
  W -> H (-1) 0 1
  NW -> H 0 (-1) 1
  NE -> H 1 (-1) 0

moves :: [HDir] -> HVec
moves = foldr move mempty

-- remember black tiles
type Floor = Set HVec

flips :: [HDir] -> Floor -> Floor
flips ds = runIdentity . Set.alterF (Identity . not) (moves ds)

-- >>> solve1 example
-- 10
solve1 :: [[HDir]] -> Int
solve1 = length . foldr flips mempty

neighbors :: HVec -> [HVec]
neighbors hv = map (`move` hv) [minBound .. maxBound]

-- zero neighbor count is ignored
neighborBCount :: Floor -> Map HVec Int
neighborBCount fl = Map.fromListWith (+) (zip ns (repeat 1))
 where
  ns = concatMap neighbors $ Set.toList fl

newDay :: Floor -> Floor
newDay fl = newBs `Set.union` newWs
 where
  bc = neighborBCount fl
  (blackBC, whiteBC) = Map.partitionWithKey (\k _ -> k `Set.member` fl) bc
  -- Any black tile with zero or more than 2 black adjancent --> white
  newBs = Map.keysSet . Map.filter (`elem` [1,2]) $ blackBC
  -- Any white tile with exactly 2 black tiles adjacent --> black
  newWs = Map.keysSet . Map.filter (== 2) $ whiteBC

-- >>> solve2 example
-- 2208
solve2 :: [[HDir]] -> Int
solve2 = length . (!! 100) . iterate newDay . foldr flips mempty

example :: [[HDir]]
example =
  parse . unlines $
    [ "sesenwnenenewseeswwswswwnenewsewsw"
    , "neeenesenwnwwswnenewnwwsewnenwseswesw"
    , "seswneswswsenwwnwse"
    , "nwnwneseeswswnenewneswwnewseswneseene"
    , "swweswneswnenwsewnwneneseenw"
    , "eesenwseswswnenwswnwnwsewwnwsene"
    , "sewnenenenesenwsewnenwwwse"
    , "wenwwweseeeweswwwnwwe"
    , "wsweesenenewnwwnwsenewsenwwsesesenwne"
    , "neeswseenwwswnwswswnw"
    , "nenwswwsewswnenenewsenwsenwnesesenew"
    , "enewnwewneswsewnwswenweswnenwsenwsw"
    , "sweneswneswneneenwnewenewwneswswnese"
    , "swwesenesewenwneswnwwneseswwne"
    , "enesenwswwswneneswsenwnewswseenwsese"
    , "wnwnesenesenenwwnenwsewesewsesesew"
    , "nenewswnwewswnenesenwnesewesw"
    , "eneswnwswnwsenenwnwnwwseeswneewsenese"
    , "neswnwewnwnwseenwseesewsenwsweewe"
    , "wseweeenwnesenwwwswnew"
    ]