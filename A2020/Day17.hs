-- |
-- Module      : Day17
-- Description : Solution to AOC 2020 Day 17: Conway Cubes
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/17>
module Day17 where

import Data.List (foldl', iterate')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

type P2 = (Int, Int)

data P3 = P3 {z3 :: Int, y3 :: Int, x3 :: Int} deriving (Eq, Ord, Show)

type Pocket = M.Map P3 ()

-- | Solution to Day 17.
main17 :: FilePath -> IO ()
main17 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

parse :: String -> Pocket
parse = M.fromList . map toP3 . parseLayer . lines
  where
    parseLayer :: [String] -> [P2]
    parseLayer = catMaybes . concat . zipWith active [0 ..]
    active :: Int -> String -> [Maybe P2]
    active i = zipWith (isActive i) [0 ..]
    isActive :: Int -> Int -> Char -> Maybe P2
    isActive i j c = if c == '.' then Nothing else Just (i, j)
    toP3 :: P2 -> (P3, ())
    toP3 = (,()) . uncurry (P3 0)

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 $ parse example
-- 112
solve1 :: Pocket -> Int
solve1 = length . snd . last . cubeCycles 6

pm :: Int -> [Int]
pm x = [pred x .. succ x]

-- | The cube and cubes in distance 1.
--
-- >>> length $ neighborhood (P3 0 0 0)
-- 27
--
-- This is an ascending list:
--
-- >>> let n = neighborhood (P3 0 0 0) in and $ zipWith (<) n (tail n)
-- True
neighborhood :: P3 -> [P3]
neighborhood P3 {..} = P3 <$> pm z3 <*> pm y3 <*> pm x3

neightbors :: P3 -> [P3]
neightbors p = filter (/= p) $ neighborhood p

changeSpace :: Pocket -> S.Set P3
changeSpace = S.unions . map (S.fromDistinctAscList . neighborhood) . M.keys

changeState :: Pocket -> P3 -> Maybe P3
changeState p p3 = bToM state
  where
    bToM b = if b then Just p3 else Nothing
    n = length . mapMaybe (p M.!?) $ neightbors p3
    state = case p M.!? p3 of
      Nothing -> n == 3
      Just () -> n == 3 || n == 2

cubesChange :: Pocket -> Pocket
cubesChange p = M.fromDistinctAscList . flip zip (repeat ()) $ next
  where
    next :: [P3]
    next = mapMaybe (changeState p) (S.elems $ changeSpace p)

cubeCycles :: Int -> Pocket -> [(Int, Pocket)]
cubeCycles n = zip [0 .. n] . iterate' cubesChange

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

example :: String
example =
  unlines
    [ ".#.",
      "..#",
      "###"
    ]

-- >>> pocketSize $ parse example
-- ((0,0),(0,2),(0,2))
pocketSize :: Pocket -> (P2, P2, P2)
pocketSize = foldl' minMax3 (ze, ze, ze) . M.keys
  where
    ze = (0, 0)
    minMax3 :: (P2, P2, P2) -> P3 -> (P2, P2, P2)
    minMax3 (mz, my, mx) (P3 z y x) = (,,) (m mz z) (m my y) (m mx x)
    m :: P2 -> Int -> P2
    m (mi, ma) i = (,) (min mi i) (max ma i)

printPocket :: Pocket -> IO ()
printPocket p = mapM_ printLayer [zmin .. zmax]
  where
    ((zmin, zmax), mx, my) = pocketSize p
    printLayer i = putStr "z=" >> print i >> putStr (showLayer p i my mx) >> putStrLn ""

showLayer :: Pocket -> Int -> P2 -> P2 -> String
showLayer p z3 yr xr = unlines $ do
  y3 <- range yr
  pure $ do
    x3 <- range xr
    pure $ mToC (M.lookup P3 {..} p)
  where
    range (l, r) = [l .. r]
    mToC = maybe '.' (const '#')

showGenerations :: Int -> Pocket -> IO ()
showGenerations n = mapM_ printer . cubeCycles n
  where
    printer :: (Int, Pocket) -> IO ()
    printer (i, p) = header i >> printPocket p
    header :: Int -> IO ()
    header i = putStr $ "\nAfter " <> show i <> " cycles:\n\n"
