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

-- | Point in three/four dimensional space - `x0` added last.
data P4 = P4 {x0 :: Int, x1 :: Int, x2 :: Int, x3 :: Int} deriving (Eq, Ord, Show)

type P3 = P4

p3 :: Int -> Int -> Int -> P3
p3 = P4 0

type Pocket = M.Map P4 ()

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
    toP3 = (,()) . uncurry (p3 0)

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 $ parse example
-- 112
solve1 :: Pocket -> Int
solve1 = length . snd . last . cubeCycles 6

pm :: Int -> [Int]
pm x = [pred x .. succ x]

-- | The cube and cubes in distance 1 in ascending order.
--
-- >>> length $ neighbourhood3 (p3 0 0 0)
-- 27
-- >>> let n = neighbourhood3 (p3 0 0 0) in and $ zipWith (<) n (tail n)
-- True
neighbourhood3 :: P3 -> [P3]
neighbourhood3 P4 {..} = p3 <$> pm x1 <*> pm x2 <*> pm x3

neighbour3 :: P3 -> [P3]
neighbour3 !p = filter (/= p) $ neighbourhood3 p

changingCubes :: Pocket -> S.Set P3
changingCubes = S.unions . map (S.fromDistinctAscList . neighbourhood3) . M.keys

changeState3 :: Pocket -> P3 -> Maybe P3
changeState3 !poc !p = bToM state
  where
    bToM b = if b then Just p else Nothing
    !n = length . mapMaybe (poc M.!?) $ neighbour3 p
    state = case poc M.!? p of
      Nothing -> n == 3
      Just () -> n == 3 || n == 2

cubesChange :: Pocket -> Pocket
cubesChange !p = M.fromDistinctAscList . flip zip (repeat ()) $ next
  where
    next :: [P3]
    next = mapMaybe (changeState3 p) (S.elems $ changingCubes p)

cubeCycles :: Int -> Pocket -> [(Int, Pocket)]
cubeCycles !n = zip [0 .. n] . iterate' cubesChange

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 $ parse example
-- 848
solve2 :: Pocket -> Int
solve2 = length . snd . last . tesseractCycles 6

-- | The cube and cubes in distance 1.
--
-- >>> length $ neighbourhood4 (P4 0 0 0 0)
-- 81
neighbourhood4 :: P4 -> [P4]
neighbourhood4 P4 {..} = P4 <$> pm x0 <*> pm x1 <*> pm x2 <*> pm x3

neighbour4 :: P3 -> [P3]
neighbour4 !p = filter (/= p) $ neighbourhood4 p

changeSpace4 :: Pocket -> S.Set P4
changeSpace4 = S.unions . map (S.fromDistinctAscList . neighbourhood4) . M.keys

changeState4 :: Pocket -> P4 -> Maybe P4
changeState4 !poc !p = bToM state
  where
    bToM !b = if b then Just p else Nothing
    !n = length . mapMaybe (poc M.!?) $ neighbour4 p
    !state = case poc M.!? p of
      Nothing -> n == 3
      Just () -> n == 3 || n == 2

tesseractChange :: Pocket -> Pocket
tesseractChange !p = M.fromDistinctAscList . flip zip (repeat ()) $ next
  where
    next :: [P4]
    next = mapMaybe (changeState4 p) (S.elems $ changeSpace4 p)

tesseractCycles :: Int -> Pocket -> [(Int, Pocket)]
tesseractCycles !n = zip [0 .. n] . iterate' tesseractChange

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
pocketSize :: Pocket -> (P2, P2, P2, P2)
pocketSize = foldl' minMax3 (ze, ze, ze, ze) . M.keys
  where
    ze = (0, 0)
    minMax3 :: (P2, P2, P2, P2) -> P4 -> (P2, P2, P2, P2)
    minMax3 (m0, m1, m2, m3) P4 {..} = (,,,) (m m0 x0) (m m1 x1) (m m2 x2) (m m3 x3)
    m :: P2 -> Int -> P2
    m (mi, ma) i = (,) (min mi i) (max ma i)

printPocket :: Pocket -> IO ()
printPocket p = mapM_ printLayer ((,) <$> range mw <*> range mz)
  where
    (mw, mz, my, mx) = pocketSize p
    printLayer (w, z) = do
      putStrLn $ "\nz=" <> show z <> ", w=" <> show w
      putStr (showLayer p w z my mx)

showLayer :: Pocket -> Int -> Int -> P2 -> P2 -> String
showLayer p x0 x1 yr xr = unlines $ do
  x2 <- range yr
  pure $ do
    x3 <- range xr
    pure $ mToC (M.lookup P4 {..} p)
  where
    mToC = maybe '.' (const '#')

range :: Enum a => (a, a) -> [a]
range (l, r) = [l .. r]

showCycles :: (Int -> Pocket -> [(Int, Pocket)]) -> Int -> Pocket -> IO ()
showCycles iter n = mapM_ printer . iter n
  where
    printer :: (Int, Pocket) -> IO ()
    printer (i, p) = header i >> printPocket p
    header :: Int -> IO ()
    header i = putStr $ "\nAfter " <> show i <> " cycles:\n\n"
