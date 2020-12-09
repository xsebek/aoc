-- |
-- Module      : Day09
-- Description : Solution to AOC 2020 Day 9: Encoding Error.
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/9>
module Day09 where

import Criterion.Main
import Data.List (tails)
import Data.Maybe (mapMaybe)

-- | Solution to Day 9.
main09 :: FilePath -> IO ()
main09 f = do
  input <- parse <$> readFile f
  print $ solve1 25 input
  print $ solve2 25 input
  defaultMain
    [ bgroup
        "09"
        [ bench "part1" $ whnf (solve1 25) input,
          bench "part2" $ whnf (solve2 25) input
        ]
    ]

parse :: String -> [Int]
parse = map read . lines

-- >>> solve1 5 example
-- 127
solve1 :: Int -> [Int] -> Int
solve1 n = snd . head . filter (null . uncurry prevSum) . previousN n

prevSum :: (Num a, Eq a) => [a] -> a -> [(a, a)]
prevSum xs x = [(i, j) | (i : ys) <- tails xs, j <- ys, i + j == x]

-- >>> previousN 3 [1..5]
-- [([1,2,3],4),([2,3,4],5)]
previousN :: Int -> [a] -> [([a], a)]
previousN n xs = zip (map (take n) $ tails xs) (drop n xs)

-- >>> solve2 5 example
-- 62
solve2 :: Int -> [Int] -> Int
solve2 n xs = sumHeadLast $ head sums
  where
    sumHeadLast rs = minimum rs + maximum rs
    sums = mapMaybe (sumToN s1) (tails xs)
    s1 = solve1 n xs

-- >>> mapMaybe (sumToN 127) $ tails example
-- [[15,25,47,40]]
sumToN :: Int -> [Int] -> Maybe [Int]
sumToN _ [] = Nothing
sumToN n (i : is) = (i :) <$> go i is
  where
    go _ [] = Nothing
    go acc (x : xs) =
      let na = acc + x
       in case compare na n of
            GT -> Nothing
            EQ -> Just [x]
            LT -> (x :) <$> go na xs

example :: [Int]
example =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]