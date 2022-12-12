-- |
-- Module      : Day12
-- Description : Solution to AOC 2022 Day 12: Hill Climbing Algorithm
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/12>
module Day12 where
import Grid qualified as G
import Grid (Grid)
import qualified Data.List as List
import Algorithm.Search (aStar, bfs)

-- | Solution to Day 12.
main12 :: FilePath -> IO ()
main12 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> Grid Char
parse = G.grid . lines

-- >>> solve1 example
-- 31
solve1 :: Grid Char -> Int
solve1 = maybe 0 fst . hikeUp

findOne :: Char -> Grid Char -> (Int, Int)
findOne c g = case List.find ((==c) . snd) $ G.assocs g of
  Just (p, _c) -> p
  Nothing -> error $ "Could not find one " <> show c

setup :: Grid Char -> (Grid Char, (Int, Int), (Int, Int))
setup og = (g, s, e)
 where
  s = findOne 'S' og
  e = findOne 'E' og
  g = og G.// [(s, 'a'), (e, 'z')]

hikeUp :: Grid Char -> Maybe (Int, [(Int, Int)])
hikeUp og = aStar
  (\p -> filter (isReachable g p) $ G.neighbors4 g p)
  (\_ _ -> 1)
  (dist end)
  (==end)
  start
 where
  (g, start, end) = setup og
  dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- >>> isReachable example (1,1) (2,1)
-- True
-- >>> isReachable example (2,0) (3,0)
-- False
isReachable :: Grid Char -> (Int, Int) -> (Int, Int) -> Bool
isReachable g p1 p2 = succ (g G.! p1) >= g G.! p2

-- >>> solve2 example
-- 29
solve2 :: Grid Char -> Int
solve2 = maybe 0 length . hikeDown

hikeDown :: Grid Char -> Maybe [(Int, Int)]
hikeDown og = bfs
  (\p -> filter (flip (isReachable g) p) $ G.neighbors4 g p)
  ((=='a') . (g G.!))
  end
 where
  (g, _start, end) = setup og

-- >>> printEx example
-- *** Exception: This is fine.
-- Sabqponm
-- abcryxxl
-- accszExk
-- acctuvwj
-- abdefghi
printEx :: Grid Char -> a
printEx = error . ("This is fine.\n" <>) . unlines . G.toList

example :: Grid Char
example = parse . unlines $
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]
