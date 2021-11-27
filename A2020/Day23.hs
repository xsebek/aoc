-- |
-- Module      : Day23
-- Description : Solution to AOC 2020 Day 23: Crab Cups
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/23>
module Day23 where

import Data.Foldable (Foldable (toList), find)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (Empty, (:<|)), (<|), (|>))
import qualified Data.Sequence as Seq

-- | Solution to Day 23.
main23 :: FilePath -> IO ()
main23 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type Circle = Seq Int

parse :: String -> Circle
parse = Seq.fromList . map (read . (: "")) . head . lines

rotate :: Circle -> Circle
rotate Empty = undefined
rotate (curr :<| rest) = rest |> curr

move :: Circle -> Circle
move Empty = undefined
move cs@(curr :<| rest) = rotate inserted
 where
  (taken, left) = Seq.splitAt 3 rest
  temp = curr <| left
  minC = minimum cs
  maxC = maximum cs
  makeP n = if n < minC then maxC + 1 - minC + n else n
  dests = map (makeP . (curr -)) [1 ..]
  destE = fromJust $ find (`elem` temp) dests
  destPos = 1 + fromJust (destE `Seq.elemIndexL` temp)
  inserted = let (l, r) = Seq.splitAt destPos temp in l <> taken <> r

circleFrom1 :: Circle -> Int
circleFrom1 c = sum $ zipWith (*) pows10 from1R
 where
  pos = fromJust (1 `Seq.elemIndexL` c)
  (l, r) = Seq.splitAt pos c
  from1R = toList . Seq.reverse . Seq.drop 1 $ r <> l
  pows10 = map (10 ^) [(0 :: Int) ..]

moves :: Circle -> [Int]
moves = map circleFrom1 . iterate move

-- >>> solve1 example
solve1 :: Circle -> Int
solve1 = (!! 100) . moves

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

example :: Circle
example = parse "389125467"