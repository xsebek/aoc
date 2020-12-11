-- |
-- Module      : Day11
-- Description : Solution to AOC 2020 Day 11: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/11>
module Day11 where

import Control.Monad (guard)
import Criterion.Main
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Vector as V

-- | Solution to Day 11.
main11 :: FilePath -> IO ()
main11 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "11"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

data Position = Floor | Empty | Full deriving (Eq, Show)

type Grid = V.Vector (V.Vector Position)

parse :: String -> Grid
parse = V.fromList . map (V.fromList . map toPos) . lines

toPos :: Char -> Position
toPos = \case
  '.' -> Floor
  'L' -> Empty
  '#' -> Full
  c -> error $ "Invalid position: " <> [c]

get :: Grid -> Int -> Int -> Maybe Position
get g x y = g V.!? y >>= (V.!? x)

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 $ parse example
-- 37
solve1 :: Grid -> Int
solve1 = countFull . simulate rule1

countFull :: Grid -> Int
countFull = V.sum . V.map (V.sum . V.map (fromEnum . (== Full)))

type Rule = Grid -> Int -> Int -> Position -> Position

-- >>> simulate rule1 (parse example) == parse exampleFinal
-- True
-- >>> simulate rule2 (parse example) == parse exampleFinal2
-- True
simulate :: Rule -> Grid -> Grid
simulate rule g = if ng == g then g else simulate rule ng
  where
    ng = nextRound rule g

nextRound :: Rule -> Grid -> Grid
nextRound rule g = V.imap (V.imap . rule g) g

rule1 :: Rule
rule1 g y x = \case
  Floor -> Floor
  Empty -> if Full `notElem` adj then Full else Empty
  Full -> if length (filter (== Full) adj) >= 4 then Empty else Full
  where
    adj = adjancent g x y

-- >>> adjancent (parse example) 0 0
-- [Empty,Floor,Empty]
adjancent :: Grid -> Int -> Int -> [Position]
adjancent g x y = mapMaybe (getPos . move x y) dirs
  where
    getPos = uncurry (get g)
    move i j (a, b) = (i + a, j + b)

dirs :: [(Int, Int)]
dirs = do
  i <- [- 1 .. 1]
  j <- [- 1 .. 1]
  guard $ (i, j) /= (0, 0)
  pure (i, j)

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 (parse example)
-- 26
solve2 :: Grid -> Int
solve2 = countFull . simulate rule2

rule2 :: Rule
rule2 g y x = \case
  Floor -> Floor
  Empty -> if vis == 0 then Full else Empty
  Full -> if vis >= 5 then Empty else Full
  where
    vis = visibleFull g x y

-- >>> visibleFull (parse ex2vis8) 3 4
-- 8
visibleFull :: Grid -> Int -> Int -> Int
visibleFull g x y = length . filter startFull $ direction g x y
  where
    startFull = (== Just Full) . listToMaybe . dropWhile (== Floor)

-- >>> map fromPosition <$> direction (parse ex2vis8) 3 4
-- [".#.","#..","..#","..#.","...#","...#","....#","#..."]
direction :: Grid -> Int -> Int -> [[Position]]
direction g x y = map (catMaybes . takeWhile isJust) pss
  where
    dir (i, j) = tail $ zip [x, x + i ..] [y, y + j ..]
    pss = map (uncurry $ get g) . dir <$> dirs

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

roundPrint :: Rule -> Grid -> IO ()
roundPrint r g = mapM_ (\(i, x) -> print i >> mapM_ putStrLn (toMap x)) roundI
  where
    roundI = zip [0 ..] rounds
    rounds = map fst . takeWhile (uncurry (/=)) $ zip rounds' (tail rounds')
    rounds' = iterate (nextRound r) g

roundN :: Rule -> Int -> Grid -> Grid
roundN rule n g = iterate (nextRound rule) g !! n

fromPosition :: Position -> Char
fromPosition = \case
  Floor -> '.'
  Empty -> 'L'
  Full -> '#'

toMap :: Grid -> [String]
toMap = map (map fromPosition . V.toList) . V.toList

parseDoPrint :: String -> (Grid -> Grid) -> IO ()
parseDoPrint s f = mapM_ putStrLn . toMap . f $ parse s

example :: String
example =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

exampleFinal :: String
exampleFinal =
  unlines
    [ "#.#L.L#.##",
      "#LLL#LL.L#",
      "L.#.L..#..",
      "#L##.##.L#",
      "#.#L.LL.LL",
      "#.#L#L#.##",
      "..L.L.....",
      "#L#L##L#L#",
      "#.LLLLLL.L",
      "#.#L#L#.##"
    ]

ex2vis8 :: String
ex2vis8 =
  unlines
    [ ".......#.",
      "...#.....",
      ".#.......",
      ".........",
      "..#L....#",
      "....#....",
      ".........",
      "#........",
      "...#....."
    ]

exampleFinal2 :: String
exampleFinal2 =
  unlines
    [ "#.L#.L#.L#",
      "#LLLLLL.LL",
      "L.L.L..#..",
      "##L#.#L.L#",
      "L.L#.LL.L#",
      "#.LLLL#.LL",
      "..#.L.....",
      "LLL###LLL#",
      "#.LLLLL#.L",
      "#.L#LL#.L#"
    ]