{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}

module Day05 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.IntSet (elems, findMax, findMin, fromList, (\\))
import Data.List (foldl')
import Solution

type Bits = [Int]

solution :: Solve [(Bits, Bits)]
solution = solve parse solve1 solve2

parse :: String -> [(Bits, Bits)]
parse = map (bimap (map fb) (map lr) . break (`elem` ['L', 'R'])) . words
  where
    fb = \case
      'F' -> 0
      'B' -> 1
      c -> error $ "The character " <> [c] <> "is not Front/Back"
    lr = \case
      'L' -> 0
      'R' -> 1
      c -> error $ "The character " <> [c] <> "is not Left/Right"

solve1 :: [(Bits, Bits)] -> Int
solve1 = maximum . map seatInt

seatInt :: (Bits, Bits) -> Int
seatInt = binToInt . uncurry (<>)

binToInt :: Bits -> Int
binToInt = foldl' ((+) . (2 *)) 0

solve2 :: [(Bits, Bits)] -> Int
solve2 = head . emptySeats . map seatInt

emptySeats :: [Int] -> [Int]
emptySeats s = elems $ continuous \\ seats
  where
    continuous = [findMin seats .. findMax seats]
    seats = fromList s