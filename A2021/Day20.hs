-- |
-- Module      : Day20
-- Description : Solution to AOC 2021 Day 20: Trench Map
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/20>
module Day20 where

import Data.Bifunctor (Bifunctor (bimap, first))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Solution to Day 20.
main20 :: FilePath -> IO ()
main20 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

newtype Enhance = E IntSet
  deriving (Eq, Show)

type P2 = (Int, Int)
type Range = (Int, Int)

data Rectangle = R {xrange :: Range, yrange :: Range}
  deriving (Eq, Ord, Show)

data Image = Img
  { frame :: Rectangle
  , img :: Set P2
  }
  deriving (Eq, Show)

parse :: String -> (Enhance, Image)
parse = bimap enhanceP imageP . splitOn2Eol
 where
  splitOn2Eol = first concat . breakBy null . lines
  enum = zip [0 ..]
  enhanceP = E . IntSet.fromAscList . map fst . filter ((== '#') . snd) . enum 
  imageP ls =
    Img (R (0, pred . length $ head ls) (0, length ls - 1)) $
      Set.fromList
        [ (x, y) | (y, l) <- enum ls, (x, c) <- enum l, c == '#'
        ]

breakBy :: (a -> Bool) -> [a] -> ([a], [a])
breakBy _ [] = ([], [])
breakBy p (x : xs) = if p x then ([], xs) else first (x :) $ breakBy p xs

lookupP2 :: Image -> P2 -> Maybe Bool
lookupP2 (Img f i) p = if inside p f then Just $ Set.member p i else Nothing

inside :: P2 -> Rectangle -> Bool
inside (x, y) R{..} = inRange x xrange && inRange y yrange
 where
  inRange i (l, r) = min l r <= i && i <= max l r

neighborhood :: Bool -> Image -> P2 -> Int
neighborhood rest im (x, y) = binToInt $ [fromMaybe rest p | j <- pm y, i <- pm x, let p = lookupP2 im (i, j)]
  where
    pm i = (i+) <$> [-1,0,1]

stepP2 :: Enhance -> Bool -> Image -> P2 -> Bool
stepP2 (E is) rest im p = IntSet.member (neighborhood rest im p) is

step :: Enhance -> Bool -> Image -> Image
step e rest = news . enlarge
 where
  enlarge (Img r1@(R xr yr) im) = let r2 = R (incS xr) (incS yr) in Img r2 (Set.union im (framing r1 r2))
  framing i1 i2 = if rest then Set.fromList (allcoords' i2) Set.\\ Set.fromList (allcoords' i1) else mempty
  incS = bimap (-2 +) (2 +)
  news im = im{img = Set.fromList [p | y <- ycoords im, x <- xcoords im, let p = (x, y), stepP2 e rest im p]}

enhancement :: Int -> (Enhance, Image) -> Image
enhancement s (e, i) = go False s i
 where
  go rest steps im =
    let nim = step e rest im
        nr = newRest e rest
     in if steps == 0 then im else go nr (pred steps) nim
  newRest :: Enhance -> Bool -> Bool
  newRest (E is) b = IntSet.member (binToInt (replicate 9 b)) is

lit :: Image -> Int
lit = Set.size . img

-- >>> solve1 example
-- 35
solve1 :: (Enhance, Image) -> Int
solve1 = lit . enhancement 2

-- >>> solve2 example
solve2 :: (Enhance, Image) -> Int
solve2 = lit . enhancement 50

pretty :: Image -> IO ()
pretty im =
  mapM_
    putStrLn
    [ [ case p of Just b -> if b then '#' else '.'; _ -> '_'
      | x <- xcoords im
      , let p = lookupP2 im (x, y)
      ]
    | y <- ycoords im
    ]

example :: (Enhance, Image)
example =
  parse . unlines $
    [ "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"
    , "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###"
    , ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#."
    , ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....."
    , ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.."
    , "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....."
    , "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
    , ""
    , "#..#."
    , "#...."
    , "##..#"
    , "..#.."
    , "..###"
    ]

binToInt :: [Bool] -> Int
binToInt = sum . zipWith (\i b -> if b then 2 ^ i else 0) [0 :: Int ..] . reverse

xcoords :: Image -> [Int]
xcoords = (\(l, r) -> [l .. r]) . xrange . frame

ycoords :: Image -> [Int]
ycoords = (\(l, r) -> [l .. r]) . yrange . frame

allcoords :: Image -> [P2]
allcoords i = (,) <$> xcoords i <*> ycoords i

allcoords' :: Rectangle -> [P2]
allcoords' i = (,) <$> xcoords (Img i mempty) <*> ycoords (Img i mempty)
