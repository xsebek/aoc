-- |
-- Module      : Day25
-- Description : Solution to AOC 2021 Day 25: Sea Cucumber
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/25>
module Day25 where

import Data.Array (assocs, bounds, (!), (//))
import Data.Array.Unboxed (Array, array)

-- | Solution to Day 25.
main25 :: FilePath -> IO ()
main25 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> SeaFloor
parse = seaFloor . concatMap (zipWith (\x (y, c) -> ((x, y), parseCell c)) [0 ..]) . zipWith (map . (,)) [0 ..] . lines

seaFloor :: [(P, Cell)] -> SeaFloor
seaFloor nncs = array (zero, maxP) nncs
 where
  maxP = fst $ last nncs

type P = (Int, Int)

zero :: P
zero = (0, 0)

data Cell = Empty | CucumberSouth | CucumberEast
  deriving (Eq, Ord, Show, Enum)

parseCell :: Char -> Cell
parseCell = \case
  '>' -> CucumberEast
  'v' -> CucumberSouth
  '.' -> Empty
  c -> error $ "Unknown character: " <> show c

cellChar :: Cell -> Char
cellChar = \case
  Empty -> '.'
  CucumberSouth -> 'v'
  CucumberEast -> '>'

type SeaFloor = Array P Cell

prettyFloor :: SeaFloor -> String
prettyFloor s =
  let ((x1, y1), (x2, y2)) = bounds s
   in unlines
        [ [ cellChar c
          | i <- [x1 .. x2]
          , let c = s ! (i, j)
          ]
        | j <- [y1 .. y2]
        ]

-- ----------------------------------------------------------------------------
-- PART 1
-- ----------------------------------------------------------------------------

-- >>> solve1 example
-- 58
solve1 :: SeaFloor -> Int
solve1 = length . moveUntilStop

moveEast :: SeaFloor -> SeaFloor
moveEast s = s // updates
 where
  (_min, (mx, _my)) = bounds s
  updates = concatMap move allEast
  allEast = map fst . filter ((== CucumberEast) . snd) $ assocs s
  move p@(x, y) =
    let pe = ((x + 1) `mod` (mx + 1), y)
     in if s ! pe == Empty then [(p, Empty), (pe, CucumberEast)] else []

moveSouth :: SeaFloor -> SeaFloor
moveSouth s = s // updates
 where
  (_min, (_mx, my)) = bounds s
  updates = concatMap move allSouth
  allSouth = map fst . filter ((== CucumberSouth) . snd) $ assocs s
  move p@(x, y) =
    let ps = (x, (y + 1) `mod` (my + 1))
     in if s ! ps == Empty then [(p, Empty), (ps, CucumberSouth)] else []

moveAll :: SeaFloor -> SeaFloor
moveAll = moveSouth . moveEast

moveUntilStop :: SeaFloor -> [SeaFloor]
moveUntilStop s =
  let ss = iterate moveAll s
      (d,e) = span (uncurry (/=)) $ zip ss (tail ss)
  in map fst $ d <> take 1 e

-- ----------------------------------------------------------------------------
-- PART 2
-- ----------------------------------------------------------------------------

-- >>> solve2 example
solve2 :: a -> Int
solve2 = errorWithoutStackTrace "Part 2 not implemented"

example :: SeaFloor
example =
  parse . unlines $
    [ "v...>>.vv>"
    , ".vv>>.vv.."
    , ">>.>v>...v"
    , ">>v>>.>.v."
    , "v>v.vv.v.."
    , ">.>>..v..."
    , ".vv..>.>v."
    , "v.v..>>v.v"
    , "....v..v.>"
    ]

exampleLine :: SeaFloor
exampleLine = parse "...>>>>>..."
