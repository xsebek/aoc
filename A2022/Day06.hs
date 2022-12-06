-- |
-- Module      : Day06
-- Description : Solution to AOC 2022 Day 06: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/06>
module Day06 where
import qualified Data.List as List

-- | Solution to Day 06.
main06 :: FilePath -> IO ()
main06 f = do
  input <- readFile f
  print $ solve1 input
  print $ solve2 input

-- >>> solve1 example
-- 7
solve1 :: String -> Int
solve1 = untilMarkerEnd 4 

untilMarkerEnd :: Int -> String -> Int
untilMarkerEnd n zs = 
  let zN = take n zs
  in if length zN /= n then error "No marker found"
    else if length (uniq zN) == n then n else 1 + untilMarkerEnd n (tail zs)

uniq :: [Char] -> [Char]
uniq = map head . List.group . List.sort

-- >>> solve2 example
-- 19
solve2 :: String -> Int
solve2 = untilMarkerEnd 14

example :: String
example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
