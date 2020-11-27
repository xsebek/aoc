{-# LANGUAGE LambdaCase #-}

-- | Solution to AOC 2015 Day 1: Not Quite Lisp
--
-- https://adventofcode.com/2015/day/1
--
-- Santa is trying to deliver presents in a large apartment building,
-- but he can't find the right floor - the directions he got are
-- a little confusing.
--
-- He starts on the ground floor (floor 0) and then follows
-- the instructions one character at a time.
--
-- An opening parenthesis, (, means he should go up one floor, and
-- a closing parenthesis, ), means he should go down one floor.
--
-- The apartment building is very tall, and the basement is very deep;
-- he will never find the top or bottom floors.
--
-- To what floor do the instructions take Santa?
module Day01 where

import Control.Monad (foldM)

-- | Process input text file.
main :: IO ()
main = do
  input <- readFile "input01.txt"
  print (solve1 input)
  print (solve2 input)

-- | Solve first task.
--
-- >>> solve1 "(())"
-- 0
-- >>> solve1 "))((((("
-- 3
-- >>> solve1 ")())())"
-- -3
solve1 :: String -> Int
solve1 = foldr move 0

move :: Char -> Int -> Int
move = \case
  '(' -> (+ 1)
  ')' -> subtract 1
  _ -> id

-- | Part Two.
--
-- Now, given the same instructions, find the position of the first
-- character that causes him to enter the basement (floor -1).
-- The first character in the instructions has position 1, the second
-- character has position 2, and so on.
--
-- >>> solve2 ")))"
-- Left 1
solve2 :: String -> Either Int (Int, Int)
solve2 = foldM moveIf (1, 0)

moveIf :: (Int, Int) -> Char -> Either Int (Int, Int)
moveIf (i, floor) c =
  let next = move c floor
   in if next == -1 then Left i else Right (i + 1, next)
