{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Day11
-- Description : Solution to AOC 2022 Day 11: Monkey in the Middle
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/11>
module Day11 where
import qualified Data.Text as T
import Data.Sequence (Seq, (|>))
import Parser (parseExample', Parser, parseFile)
import Data.IntMap (IntMap)
import qualified Parser as P
import qualified Data.IntMap.Lazy as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as List
import Prelude hiding (round)

-- | Solution to Day 11.
main11 :: FilePath -> IO ()
main11 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

-- ----------------------------------------------------------------------------
-- TYPES
-- ----------------------------------------------------------------------------

data Monkey = M
  { inspections :: Int
  , items :: Seq Int
  , operation :: SimpleExpr
  , testDiv :: Int
  , testTrue :: MonkeyIx
  , testFalse :: MonkeyIx
  }
  deriving (Eq, Ord, Show)

type MonkeyIx = Int

data Val = Old | N Int
  deriving (Eq, Ord, Show)

data Op = Add | Mul
  deriving (Eq, Ord, Show)

data SimpleExpr = New Val Op Val
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------------------
-- PARSER
-- ----------------------------------------------------------------------------

parser :: Parser (IntMap Monkey)
parser = M.fromList <$> monkeyParser `P.sepBy` P.eol

monkeyParser :: Parser (MonkeyIx, Monkey)
monkeyParser = (,) <$> (P.string "Monkey " *> P.decimal <* P.string ":\n") <*> monkey
 where
  monkey :: Parser Monkey
  monkey = M 0 <$> si <*> op <*> d <*> throwIf "true" <*> throwIf "false"
  si = P.space *> P.string "Starting items: " *> numberSeq <* P.eol
  op = P.space *> P.string "Operation: " *> exprParser <* P.eol
  d = P.space *> P.string "Test: divisible by " *> P.decimal <* P.eol
  numberSeq = Seq.fromList <$> P.decimal `P.sepBy` P.string ", "
  throwIf :: Text -> Parser MonkeyIx
  throwIf p = P.space *> P.string ("If " <> p <> ": throw to monkey ") *> P.decimal <* P.eol

-- >>> parseExample' exprParser "new = old * 19"
-- New Old Mul (N 19)
exprParser :: Parser SimpleExpr
exprParser = P.string "new = " *> (New <$> v <*> o <*> v)
 where
  v = (Old <$ P.string "old") <|> (N <$> P.decimal)
  o = P.space *> ((Add <$ P.char '+') <|> (Mul <$ P.char '*')) <* P.space

-- ----------------------------------------------------------------------------
-- PART 1
-- ----------------------------------------------------------------------------

-- >>> solve1 example
-- 10605
solve1 :: IntMap Monkey -> Int
solve1 = product . take 2 . reverse . List.sort . map inspections . M.elems . (!!20) . iterate (round False)

round :: Bool -> IntMap Monkey -> IntMap Monkey
round worryLot im = List.foldl' inspection im (M.keys im)
 where
  maxWorry = if worryLot then product $ testDiv <$> im else 0
  inspection :: IntMap Monkey -> MonkeyIx -> IntMap Monkey
  inspection oldMap mix = oldMap
    & M.adjust throwItems mix
    & flip (List.foldl' $ flip catchItem) (inspect maxWorry $ oldMap M.! mix)
  throwItems :: Monkey -> Monkey
  throwItems om = om {items = Seq.empty, inspections = inspections om + length (items om)}
  catchItem :: (MonkeyIx, Int) -> IntMap Monkey -> IntMap Monkey
  catchItem (mix, item) = M.adjust (\om -> om {items = items om |> item}) mix

-- | Monkey inspects the items to throw.
--
-- Note that 0 is a special value for the first part.
--
-- >>> inspect 0 (example M.! 0)
-- [(3,500),(3,620)]
inspect :: Int -> Monkey -> [(MonkeyIx, Int)]
inspect maxWorry (M _is i o d t f) = map ins $ toList i
  where
    ins worry =
      let evalWorry = evalExpr worry o
          newWorry = if maxWorry == 0 then evalWorry `div` 3 else evalWorry `mod` maxWorry
       in (if newWorry `mod` d == 0 then (t,) else (f,)) newWorry

evalExpr :: Int -> SimpleExpr -> Int
evalExpr old (New v1 op v2) = v v1 `o`  v v2
 where
  v Old = old
  v (N i) = i
  o = case op of
    Add -> (+)
    Mul -> (*)

-- ----------------------------------------------------------------------------
-- PART 2
-- ----------------------------------------------------------------------------

-- >>> solve2 example
-- 2713310158
solve2 :: IntMap Monkey -> Int
solve2 = product . take 2 . reverse . List.sort . map inspections . M.elems . (!!10_000) . iterate (round True)

-- ----------------------------------------------------------------------------
-- EXAMPLE
-- ----------------------------------------------------------------------------

-- >>> startItems <$> example
-- fromList [(0,fromList [79,98]),(1,fromList [54,65,75,74]),(2,fromList [79,60,97]),(3,fromList [74])]
example :: IntMap Monkey
example = parseExample' parser . T.unlines $
  [ "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  , "Monkey 2:"
  , "  Starting items: 79, 60, 97"
  , "  Operation: new = old * old"
  , "  Test: divisible by 13"
  , "    If true: throw to monkey 1"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 3:"
  , "  Starting items: 74"
  , "  Operation: new = old + 3"
  , "  Test: divisible by 17"
  , "    If true: throw to monkey 0"
  , "    If false: throw to monkey 1"
  ]
