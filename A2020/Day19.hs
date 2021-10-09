-- |
-- Module      : Day19
-- Description : Solution to AOC 2020 Day 19: Monster Messages
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/19>
module Day19 where

import qualified Data.IntMap as Map
import Data.List (minimumBy)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
    ( anySingle,
      runParser,
      errorBundlePretty,
      sepBy,
      (<|>),
      Parsec,
      MonadParsec(eof),
      ParseErrorBundle )
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Rules = Map.IntMap [Rule]

-- | Solution to Day 19.
main19 :: FilePath -> IO ()
main19 f = do
  input <- parse =<< readFile f
  print $ uncurry solve1 input
  print $ uncurry solve2 input

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

type Parser = Parsec Void String

-- >>> parse example
-- (fromList [(0,[L [4,1,5]]),(1,[L [2,3],L [3,2]]),(2,[L [4,4],L [5,5]]),(3,[L [4,5],L [5,4]]),(4,[C 'a']),(5,[C 'b'])],["ababbb","bababa","abbbab","aaabbb","aaaabbb"])
parse :: MonadFail m => String -> m (Rules, [String])
parse s = (,l) <$> rules
  where
    (r, [] : l) = break null $ lines s
    rules = Map.fromList <$> mapM (parseOrFail parseRule) r

data Rule = C Char | L [Int] deriving (Eq, Show)

-- >>> parseOrFail parseRule "2: 1 3 | 3 1"
-- (2,[L [1,3],L [3,1]])
-- >>> parseOrFail parseRule "3: \"b\""
-- (3,[C 'b'])
parseRule :: Parser (Int, [Rule])
parseRule =
  (,)
    <$> sp (decimal <* char ':')
    <*> ((singleChar <|> links) `sepBy` sp (char '|'))
  where
    sp :: Parser a -> Parser a
    sp p = p <* space
    links :: Parser Rule
    links = L <$> sp decimal `sepBy` space
    singleChar :: Parser Rule
    singleChar = C <$> sp (char '"' *> anySingle <* char '"')

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 exampleRules exampleStrings
-- 2
solve1 :: Rules -> [String] -> Int
solve1 r = length . filter null . map (check r)

-- >>> map (check exampleRules) exampleStrings
-- ["","bababa","","aaabbb","b"]
check :: Rules -> String -> String
check rules s = shortest . (s :) $ checkRule rules (rules Map.! 0) s

-- >>> checkRule exampleRules [L [4,1,5]] "ababbb"
-- [""]
checkRule :: Rules -> [Rule] -> String -> [String]
checkRule rules rss s = case rss of
  [] -> []
  (r : rs) -> checkSub rules r s <> checkRule rules rs s

-- >>> checkSub exampleRules (L [4]) "ababbb"
-- ["babbb"]
checkSub :: Rules -> Rule -> String -> [String]
checkSub rules r s = case r of
  C c -> [tail s | not (null s) && c == head s]
  L [] -> [s]
  L (l : ls) -> concatMap (checkSub rules (L ls)) (checkRule rules (rules Map.! l) s)

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 exampleRules2 exampleStrings2
-- 12
solve2 :: Rules -> [String] -> Int
solve2 = solve1 . changeRules

-- | Completely replace rules 8 and 11.
--
-- > 8: 42 | 42 8
-- > 11: 42 31 | 42 11 31
changeRules :: Rules -> Rules
changeRules =
  Map.union $
    Map.fromList
      [ (8, [L [42], L [42, 8]]),
        (11, [L [42, 31], L [42, 11, 31]])
      ]

--------------------------------------------------------------------
--                            MISC                                --
--------------------------------------------------------------------

parseOrFail :: MonadFail m => Parser a -> String -> m a
parseOrFail p = eitherPretty . runParser (p <* eof) "Day19"

eitherPretty :: MonadFail m => Either (ParseErrorBundle String Void) a -> m a
eitherPretty = either (fail . errorBundlePretty) pure

shortest :: [[a]] -> [a]
shortest [] = error "empty sequence"
shortest [a] = a
shortest ([] : _) = []
shortest (x : y : xs) = shortest $ minimumBy shorter [x, y] : xs

shorter :: [a] -> [a] -> Ordering
shorter [] [] = EQ
shorter [] _ = LT
shorter _ [] = GT
shorter (_ : l) (_ : r) = shorter l r

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

example :: String
example = concat [unlines exampleRules', "\n", unlines exampleStrings]

exampleRules' :: [String]
exampleRules' =
  [ "0: 4 1 5",
    "1: 2 3 | 3 2",
    "2: 4 4 | 5 5",
    "3: 4 5 | 5 4",
    "4: \"a\"",
    "5: \"b\""
  ]

exampleRules :: Rules
exampleRules = Map.fromList . fromJust $ mapM (parseOrFail parseRule) exampleRules'

exampleStrings :: [String]
exampleStrings =
  [ "ababbb",
    "bababa",
    "abbbab",
    "aaabbb",
    "aaaabbb"
  ]

example2 :: String
example2 = concat [unlines exampleRules2', "\n", unlines exampleStrings2]

exampleRules2 :: Rules
exampleRules2 = Map.fromList . fromJust $ mapM (parseOrFail parseRule) exampleRules2'

exampleRules2' :: [String]
exampleRules2' =
  [ "0: 8 11",
    "1: \"a\"",
    "2: 1 24 | 14 4",
    "3: 5 14 | 16 1",
    "4: 1 1",
    "5: 1 14 | 15 1",
    "6: 14 14 | 1 14",
    "7: 14 5 | 1 21",
    "8: 42",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "11: 42 31",
    "12: 24 14 | 19 1",
    "13: 14 3 | 1 12",
    "14: \"b\"",
    "15: 1 | 14",
    "16: 15 1 | 14 14",
    "17: 14 2 | 1 7",
    "18: 15 15",
    "19: 14 1 | 14 14",
    "20: 14 14 | 1 15",
    "21: 14 1 | 1 14",
    "22: 14 14",
    "23: 25 1 | 22 14",
    "24: 14 1",
    "25: 1 1 | 1 14",
    "26: 14 22 | 1 20",
    "27: 1 6 | 14 18",
    "28: 16 1",
    "31: 14 17 | 1 13",
    "42: 9 14 | 10 1"
  ]

exampleStrings2 :: [String]
exampleStrings2 =
  [ "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]