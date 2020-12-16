-- |
-- Module      : Day16
-- Description : Solution to AOC 2020 Day 16: Ticket Translation
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/16>
module Day16 where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Foldable (Foldable (foldl'))
import qualified Data.IntSet as S
import Data.List (delete, sortOn)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser = Parsec Void String

type Range = (Int, Int)

type Ticket = [Int]

data Field = Field
  { name :: String,
    left :: Range,
    right :: Range
  }
  deriving (Eq, Ord)

type Input = ([Field], Ticket, [Ticket])

-- | Solution to Day 16.
main16 :: FilePath -> IO ()
main16 f = do
  input <- parse =<< readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "16"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

parse :: MonadFail m => String -> m Input
parse = parseOrFail inputParser

-- >>> parseOrFail inputParser example
-- ([class: 1-3 or 5-7,row: 6-11 or 33-44,seat: 13-40 or 45-50],[7,1,14],[[7,3,47],[40,4,50],[55,2,20],[38,6,12]])
inputParser :: Parser Input
inputParser = do
  fields <- parseField `endBy` MC.eol
  _ <- nextSection "your ticket:"
  my <- parseTicket <* MC.eol
  _ <- nextSection "nearby tickets:"
  others <- parseTicket `endBy` MC.eol
  pure (fields, my, others)
  where
    nextSection s = optional MC.eol *> MC.string s <* MC.eol

-- >>> parseOrFail parseField "class: 1-3 or 5-7"
-- class: 1-3 or 5-7
parseField :: Parser Field
parseField = do
  name <- anySingleBut '\n' `manyTill` MC.char ':'
  _ <- MC.space
  left <- parseRange
  _ <- MC.space *> MC.string "or" <* MC.space
  right <- parseRange
  pure $ Field {..}
  where
    parseRange = (,) <$> MCL.decimal <* MC.char '-' <*> MCL.decimal

parseTicket :: Parser Ticket
parseTicket = (MCL.decimal `sepBy` MC.char ',') <?> "Parsing ticket"

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 <$> parse example
-- 71
solve1 :: Input -> Int
solve1 (f, _, o) = sum . filter (`S.notMember` valid) $ concat o
  where
    valid = S.unions $ map validValues f

validValues :: Field -> S.IntSet
validValues Field {..} = rangeSet left `S.union` rangeSet right
  where
    rangeSet (x, y) = S.fromDistinctAscList [x .. y]

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 <$> parse example2
-- 0
solve2 :: Input -> Int
solve2 i@(_, my, _) = product $ map fst departures
  where
    solution = eliminate (workOutNames i)
    fields = zip my solution
    departures = filter ((== "departure") . take 9 . snd) fields

type NameMap = M.Map String S.IntSet

-- >>> parse example2 >>= \(fs,_,_) -> pure (nameMap fs)
-- fromList [("class",fromList [0,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),("row",fromList [0,1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19]),("seat",fromList [0,1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19])]
nameMap :: [Field] -> NameMap
nameMap fs = M.fromList $ zip (map name fs) (map validValues fs)

-- >>> parse example2 >>= \(fs,_,_) -> pure . M.keys $ filterMap 15 (nameMap fs)
-- ["class","row"]
filterMap :: Int -> NameMap -> NameMap
filterMap = M.filter . S.member

-- >>> workOutNames <$> parse example2
-- [["row"],["class","row"],["class","row","seat"]]
workOutNames :: Input -> [[String]]
workOutNames (fs, _, os) = map M.keys validMaps
  where
    nM :: NameMap
    !nM = nameMap fs
    maps :: [NameMap]
    maps = repeat nM
    valid :: S.IntSet
    !valid = S.unions $ M.elems nM
    others :: [Ticket]
    others = filter (all (`S.member` valid)) os
    validMaps :: [NameMap]
    validMaps = foldl' (flip $ zipWith filterMap) maps others

-- >>> eliminate . workOutNames <$> parse example2
-- ["row","class","seat"]
eliminate :: [[String]] -> [String]
eliminate = map snd . sortOn fst . go . zip [0 ..]
  where
    go :: [(Int, [String])] -> [(Int, String)]
    go [] = []
    go iss =
      let ((i, [x]) : xss) = sortOn (length . snd) iss
       in (i, x) : go (map (delete x <$>) xss)

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

example :: String
example =
  unlines
    [ "class: 1-3 or 5-7",
      "row: 6-11 or 33-44",
      "seat: 13-40 or 45-50",
      "",
      "your ticket:",
      "7,1,14",
      "",
      "nearby tickets:",
      "7,3,47",
      "40,4,50",
      "55,2,20",
      "38,6,12"
    ]

example2 :: String
example2 =
  unlines
    [ "class: 0-1 or 4-19",
      "row: 0-5 or 8-19",
      "seat: 0-13 or 16-19",
      "",
      "your ticket:",
      "11,12,13",
      "",
      "nearby tickets:",
      "3,9,18",
      "15,1,5",
      "5,14,9"
    ]

parseOrFail :: MonadFail m => Parser a -> String -> m a
parseOrFail p = eitherPretty . runParser p "Day15"

eitherPretty :: MonadFail m => Either (ParseErrorBundle String Void) a -> m a
eitherPretty = either (fail . errorBundlePretty) pure

instance Show Field where
  show Field {..} = name <> ": " <> showR left <> " or " <> showR right
    where
      showR (x, y) = show x <> "-" <> show y