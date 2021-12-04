-- |
-- Module      : Day04
-- Description : Solution to AOC 2021 Day 04: Giant Squid
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/04>
module Day04 where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Bifunctor
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Solution to Day 04.
main04 :: FilePath -> IO ()
main04 f = do
  input <- parse <$> T.readFile f
  print $ solve1 input
  print $ solve2 input

newtype Grid = Grid (Array (Int, Int) Int)
  deriving (Eq)

instance Show Grid where
  show (Grid a) =
    "gridFromText \""
      <> intercalate
        "\\n"
        [ unwords
          [ show (a ! (x, y))
          | x <- [0 .. 4]
          ]
        | y <- [0 .. 4]
        ]
      <> "\""

gridFromText :: Text -> Grid
gridFromText t = Grid $ A.array ((0, 0), (4, 4)) ig
 where
  g = map (map readT . T.words) $ T.lines t
  ig =
    [ ((x, y), n)
    | (y, ns) <- zip [0 ..] g
    , (x, n) <- zip [0 ..] ns
    ]

data Bingo = Bingo [Int] [Grid]
  deriving (Eq, Show)

readT :: Read a => Text -> a
readT = read . T.unpack

parse :: Text -> Bingo
parse t = Bingo nums grids
 where
  (tnums, tgrids) = T.drop 2 <$> T.breakOn "\n" t
  nums = map readT $ T.splitOn "," tnums
  grids = map gridFromText $ T.splitOn "\n\n" tgrids

anyRow :: (Int -> Bool) -> Grid -> Bool
anyRow p (Grid a) = any and [[p (a ! (x, y)) | x <- [0 .. 4]] | y <- [0 .. 4]]

anyCol :: (Int -> Bool) -> Grid -> Bool
anyCol p (Grid a) = any and [[p (a ! (x, y)) | y <- [0 .. 4]] | x <- [0 .. 4]]

bingoRound :: (Set Int, Bingo) -> Either (Set Int, Grid) (Set Int, Bingo)
bingoRound (s, Bingo nums gs) = case nums of
  [] -> error "No more numbers to draw!"
  n : ns ->
    let news = Set.insert n s
        inS i = i `elem` news
        mg = find (\g -> anyCol inS g || anyRow inS g) gs
     in bimap (news,) (news,) $ maybe (Right $ Bingo ns gs) Left mg

bingoGame :: (Set Int, Bingo) -> (Int, Set Int, Grid)
bingoGame sb@(_, Bingo ns _) = case bingoRound sb of
  Left (s, gr) -> (head ns, s, gr)
  Right newSB -> bingoGame newSB

evalGame :: (Int, Set Int, Grid) -> Int
evalGame (n, s, Grid a) = n * sum (filter (`notElem` s) $ A.elems a)

-- >>> solve1 example
-- 4512
solve1 :: Bingo -> Int
solve1 = evalGame . bingoGame . (mempty,)

firstBingoRounds :: (Set Int, Bingo) -> (Set Int, Bingo)
firstBingoRounds (s, Bingo nums gs) = case nums of
  [] -> error "No more numbers to draw!"
  n : ns ->
    let news = Set.insert n s
        inS i = i `elem` news
        newGs = filter (\g -> not $ anyCol inS g || anyRow inS g) gs
     in (news, Bingo ns newGs)

longBingoGame :: (Set Int, Bingo) -> (Int, Set Int, Grid)
longBingoGame sb@(_, Bingo ns gs) =
  let newSB@(s, Bingo _ newGs) = firstBingoRounds sb
   in if null newGs then (head ns, s, head gs) else longBingoGame newSB

-- >>> solve2 example
-- 1924
solve2 :: Bingo -> Int
solve2 = evalGame . longBingoGame . (mempty,)

example :: Bingo
example = parse exampleT

exampleT :: Text
exampleT =
  T.unlines
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    , ""
    , "22 13 17 11  0"
    , " 8  2 23  4 24"
    , "21  9 14 16  7"
    , " 6 10  3 18  5"
    , " 1 12 20 15 19"
    , ""
    , " 3 15  0  2 22"
    , " 9 18 13 17  5"
    , "19  8  7 25 23"
    , "20 11 10 24  4"
    , "14 21 16 12  6"
    , ""
    , "14 21 17 24  4"
    , "10 16 15  9 19"
    , "18  8 23 26 20"
    , "22 11 13  6  5"
    , " 2  0 12  3  7"
    ]
