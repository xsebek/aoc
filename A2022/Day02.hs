-- |
-- Module      : Day02
-- Description : Solution to AOC 2022 Day 02: Rock Paper Scissors
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/02>
module Day02 where

-- | Solution to Day 02.
main02 :: FilePath -> IO ()
main02 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [(L, R)]
parse = map (two . words) . lines
 where
  two [l, r] = (read l, read r)
  two ls = error $ "Expected two words, got: " <> show ls

data L = A | B | C
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data R = X | Y | Z
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- >>> solve1 example
-- 15
solve1 :: [(L, R)] -> Int
solve1 = sum . map score1

-- >>> map score1 example
-- [8,1,6]
score1 :: (L, R) -> Int
score1 lr = scoreShape + scorePlay
 where
  scoreShape = 1 + fromEnum (snd lr)
  scorePlay = case play lr of
    LT -> 0
    EQ -> 3
    GT -> 6

-- >>> map play example
-- [GT,LT,EQ]
play :: (L, R) -> Ordering
play (l, r) = compare 1 $ (fromEnum l - fromEnum r + 1) `mod` 3

-- >>> solve2 example
-- 12
solve2 :: [(L, R)] -> Int
solve2 = sum . map score2

-- >>> map score2 example
-- [4,1,7]
score2 :: (L, R) -> Int
score2 lr = scoreShape + scorePlay
 where
  scoreShape = 1 + fromEnum (replay lr)
  scorePlay = case snd lr of
    X -> 0
    Y -> 3
    Z -> 6

-- >>> map replay example
-- [A,A,A]
replay :: (L, R) -> L
replay (l, r) = toEnum $ (fromEnum l + fromEnum r - 1) `mod` 3

example :: [(L, R)]
example =
  parse . unlines $
    [ "A Y"
    , "B X"
    , "C Z"
    ]
