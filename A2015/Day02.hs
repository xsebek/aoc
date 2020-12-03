-- | Solution to AOC 2015 Day 2: I Was Told There Would Be No Math
--
-- https://adventofcode.com/2015/day/2
--
-- The elves are running low on wrapping paper, and so they need to submit an
-- order for more.
--
-- They have a list of the dimensions (length l, width w, and height h) of each
-- present, and only want to order exactly as much as they need.
--
-- Fortunately, every present is a box (a perfect right rectangular prism),
-- which makes calculating the required wrapping paper for each gift easier:
-- find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l.
--
-- The elves also need a little extra paper for each present:
-- the area of the smallest side.
module Day02 where

import Data.List (sort, unfoldr)

-- | Process input text file.
main :: IO ()
main = do
  input <- readFile "input02.txt"
  print (sum $ map solve1 $ lines input)
  print (sum $ map solve2 $ lines input)

type Box = (Int, Int, Int)

solve1 :: String -> Int
solve1 = wrapping . parseBox

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = unfoldr (splitMaybe a)
  where
    splitMaybe :: Eq a => a -> [a] -> Maybe ([a], [a])
    splitMaybe x xs = if null xs then Nothing else Just $ split x xs

split :: Eq a => a -> [a] -> ([a], [a])
split a as = rmSep <$> break (== a) as
  where
    rmSep [] = []
    rmSep (x : xs) = if a /= x then x : xs else xs

wrapping :: Box -> Int
wrapping = (\b -> 2 * sum b + minimum b) . halfbox

-- | Part Two.
--
-- The elves are also running low on ribbon. Ribbon is all the same width,
-- so they only have to worry about the length they need to order, which they
-- would again like to be exact.
--
-- The ribbon required to wrap a present is the shortest distance around its
-- sides, or the smallest perimeter of any one face. Each present also requires
-- a bow made out of ribbon as well; the feet of ribbon required for the
-- perfect bow is equal to the cubic feet of volume of the present.
--
-- Don't ask how they tie the bow, though; they'll never tell.
--
-- >>> solve2 "2x3x4"
-- 34
solve2 :: String -> Int
solve2 = ribbon . parseBox

-- | Required feet of ribbon for a present.
--
-- >>> ribbon (1,1,10)
-- 14
ribbon :: Box -> Int
ribbon = (\b -> 2 * sum (take 2 $ sort b) + product b) . toList
  where
    toList (l, w, h) = [l, w, h]

----------------------------------------------------------------
-- HELPERS
----------------------------------------------------------------

-- | Parse (1,2,3) from "1x2x3"
--
-- >>> parseBox "1x1x10"
-- (1,1,10)
parseBox :: String -> Box
parseBox line =
  let [l, w, h] = map read $ splitOn 'x' line
   in (l, w, h)

-- | Half of box sides.
halfbox :: Box -> [Int]
halfbox (l, w, h) = [l * w, w * h, h * l]