-- |
-- Module      : Day09
-- Description : Solution to AOC 2021 Day 09: Smoke Basin
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/09>
module Day09 where

import Data.Array (Array, (!))
import Data.Array qualified as A
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (transpose, sortOn)

-- | Solution to Day 09.
main09 :: FilePath -> IO ()
main09 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type P2 = (Int, Int)

type HeightMap = Array P2 Int

parse :: String -> HeightMap
parse s = A.listArray ((0, 0), (maxX, maxY)) (concat $ transpose is)
 where
  is = map (map (read . (: ""))) $ lines s
  maxX = maximum (map length is) - 1
  maxY = length is - 1

adjancent :: HeightMap -> P2 -> [P2]
adjancent h (x,y) = filter (A.inRange $ A.bounds h) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lowPoint :: HeightMap -> P2 -> Bool
lowPoint h i = all (\j -> h ! j > h ! i) $ adjancent h i
    

lowPoints :: HeightMap -> [P2]
lowPoints h = filter (lowPoint h) (A.indices h)

-- >>> solve1 example
-- 15
solve1 :: HeightMap -> Int
solve1 h = let lps = lowPoints h in length lps + sum (map (h !) lps)

basin :: HeightMap -> P2 -> Set P2
basin h p = flood (Set.singleton p) 
 where
   flood :: Set P2 -> Set P2
   flood sp =
     let next = concatMap (\x -> map (x,) $ adjancent h x) $ Set.toList sp
         new = filter ((`notElem` sp) . snd) next
         new' = filter (\(po, pn) -> h ! pn /= 9 && h ! po < h ! pn) new
         news = sp `Set.union` Set.fromList (map snd new')
     in if null new' then sp else flood news

-- >>> solve2 example
-- 1134
solve2 :: HeightMap -> Int
solve2 h = product $ take 3 bs
  where
    lps = lowPoints h
    bs = sortOn negate $ map (length . basin h) lps

example :: HeightMap
example =
  parse . unlines $
    [ "2199943210"
    , "3987894921"
    , "9856789892"
    , "8767896789"
    , "9899965678"
    ]