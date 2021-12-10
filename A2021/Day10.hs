-- |
-- Module      : Day10
-- Description : Solution to AOC 2021 Day 10: Syntax Scoring
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/10>
module Day10 where

import Data.Either (partitionEithers)
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Maybe (listToMaybe)

-- | Solution to Day 10.
main10 :: FilePath -> IO ()
main10 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [String]
parse = lines

match :: Char -> Either Char Char
match = \case
  '(' -> Right ')'
  '[' -> Right ']'
  '{' -> Right '}'
  '<' -> Right '>'
  ')' -> Left '('
  ']' -> Left '['
  '}' -> Left '{'
  '>' -> Left '<'
  c -> error $ c : "is an unknown character"

firstWrong :: [Char] -> String -> Either Char String
firstWrong stack = \case
  [] -> Right stack
  c : s -> case match c of
    Left _c' -> if Just c /= listToMaybe stack then Left c else firstWrong (tail stack) s
    Right c' -> firstWrong (c' : stack) s

score :: Char -> Int
score = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  '(' -> 1
  '[' -> 2
  '{' -> 3
  '<' -> 4
  c -> error $ c : "is an unknown character"

-- >>> solve1 example
-- 26397
solve1 :: [String] -> Int
solve1 = sum . map (either score (const 0) . firstWrong [])

completeScore :: String -> Int
completeScore = foldl' (\i c -> 5 * i + mscore c) 0
 where
  mscore c = case match c of
    Left c' -> score c'
    Right _ -> error $ c : " is unexpected"

-- >>> solve2 example
-- 288957
solve2 :: [String] -> Int
solve2 ls = sort xs !! mid
 where
  mid = length xs `div` 2
  xs = mapEither (fmap completeScore . firstWrong []) ls
  mapEither f = snd . partitionEithers . map f

example :: [String]
example =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]