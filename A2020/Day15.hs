-- |
-- Module      : Day15
-- Description : Solution to AOC 2020 Day 15: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/15>
module Day15 where

import qualified Data.IntMap.Strict as Map

type Spoken = Map.IntMap [Int]

-- | Solution to Day 15.
main15 :: FilePath -> IO ()
main15 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

-- >>> parse example
-- [0,3,6]
parse :: String -> [Int]
parse = map read . words . map (\c -> if c == ',' then ' ' else c)

type Ix = Int

-- >>> solve1 $ parse example
-- 436
solve1 :: [Int] -> Ix
solve1 = fst . head . runUntil 2020

-- >>> map fst $ runUntil 10 $ parse example
-- [0,4,0,1,3,3,0,6,3,0]
runUntil ::
  Int -> -- stop at index
  [Int] -> -- input
  [(Int, Ix)] -- (Number, Index)
runUntil i input = newUntil i m rev
  where
    rev = reverse $ zip input [1 ..]
    m = Map.fromList $ tail $ map (fmap (: [])) rev

newUntil :: Int -> Spoken -> [(Int, Ix)] -> [(Int, Ix)]
newUntil i sp r = if snd (head nr) == i then nr else newUntil i nsp nr
  where
    (nsp, nr) = new sp r
    new sp res@((num, ix) : _) = (: res) . (,succ ix) <$> nextNumber sp num ix

-- >>> nextNumber (Map.fromList [(0,[1]), (3,[2])]) 6 3
-- (fromList [(0,[1]),(3,[2]),(6,[3])],0)
nextNumber :: Spoken -> Int -> Ix -> (Spoken, Int)
nextNumber spoken num ix = (newSpoken, next)
  where
    next = case Map.findWithDefault [] num spoken of
      [] -> 0 --error $ unwords [show index, show num]
      (x : _) -> ix - x -- error $ unwords [show index, show num, show x]
    newSpoken = Map.alter (Just . maybe [ix] (ix :)) num spoken

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

example :: String
example = "0,3,6"