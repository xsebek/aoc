-- |
-- Module      : Day13
-- Description : Solution to AOC 2021 Day 13: Transparent Origami
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/13>
module Day13 where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import Parser
import Text.Megaparsec (sepEndBy1)
import Data.Foldable (foldl')

-- | Solution to Day 13.
main13 :: FilePath -> IO ()
main13 f = do
  input <- parseFile parser f
  print $ solve1 input
  solve2 input

type Paper = IntMap IntSet

data PaperFold = FoldX Int | FoldY Int
  deriving (Show, Eq, Ord)

parser :: Parser (Paper, [PaperFold])
parser = do
  ps <- parserPoint `sepEndBy1` eol
  _ <- eol
  fs <- parserPaperFold `sepEndBy1` eol
  pure (lToMS ps, fs)
 where
  lToMS :: [(Int, Int)] -> Paper
  lToMS = Map.fromListWith Set.union . map (fmap Set.singleton . swap)

parserPoint :: Parser (Int, Int)
parserPoint = (,) <$> decimal <*> (string "," *> decimal)

parserPaperFold :: Parser PaperFold
parserPaperFold = string "fold along " *> (char 'x' $> FoldX <|> char 'y' $> FoldY) <*> (char '=' *> decimal)

-- ------------------------------------------------------------------
-- PART 1
-- ------------------------------------------------------------------

paperFoldY :: Int -> Paper -> Paper
paperFoldY i p = Map.unionWith Set.union l revR
 where
  (l, r) = Map.split i p
  revR = Map.mapKeys (abs . (2 * i -)) r

paperFoldX :: Int -> Paper -> Paper
paperFoldX i = fmap foldX
 where
  foldX s = Set.union l revR
   where
    (l, r) = Set.split i s
    revR = Set.map (abs . (2 * i -)) r

paperFold :: Paper -> PaperFold -> Paper
paperFold p = \case
 FoldX x -> paperFoldX x p
 FoldY y -> paperFoldY y p

-- >>> solve1 <$> example
-- 17
solve1 :: (Paper, [PaperFold]) -> Int
solve1 (p, fs) = sum . Map.map Set.size $ foldl' paperFold p (take 1 fs)

-- >>> solve2 =<< example
-- #####
-- #...#
-- #...#
-- #...#
-- #####
solve2 :: (Paper, [PaperFold]) -> IO ()
solve2 = prettyPaper . uncurry (foldl' paperFold)

prettyPaper :: Paper -> IO ()
prettyPaper p = mapM_ putStrLn [[if Set.member x s then '#' else '.' | x <- [minX .. maxX]] | s <- map getS [minY .. maxY]]
 where
  minY = fst $ Map.findMin p
  minX = minimum . map Set.findMin $ Map.elems p
  maxY = fst $ Map.findMax p
  maxX = maximum . map Set.findMax $ Map.elems p
  getS y = Map.findWithDefault mempty y p

example :: IO (Paper, [PaperFold])
example =
  parseExample parser . T.unlines $
    [ "6,10"
    , "0,14"
    , "9,10"
    , "0,3"
    , "10,4"
    , "4,11"
    , "6,0"
    , "6,12"
    , "4,1"
    , "0,13"
    , "10,12"
    , "3,4"
    , "3,0"
    , "8,4"
    , "1,10"
    , "2,14"
    , "8,10"
    , "9,0"
    , ""
    , "fold along y=7"
    , "fold along x=5"
    ]
