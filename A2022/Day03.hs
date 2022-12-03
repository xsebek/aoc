-- |
-- Module      : Day03
-- Description : Solution to AOC 2022 Day 03: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/03>
module Day03 where
import Data.Char (ord)
import qualified Data.Set as Set
import Data.List (foldl1')
import Data.List.Split (chunksOf)

-- | Solution to Day 03.
main03 :: FilePath -> IO ()
main03 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [(String, String)]
parse = map bag . lines
 where
  bag s = splitAt (length s `div` 2) s

-- >>> solve1 example
-- 157
solve1 :: [(String, String)] -> Int
solve1 = sum . map (priority . getOneWrong)

-- >>> map (priority . getOneWrong) example
-- [16,38,42,22,20,19]
priority :: Char -> Int
priority c = ord c - oa 
 where
  oa = if c < 'a' then ord 'A' - 27 else ord 'a' - 1

getOne :: [Char] -> Char
getOne = \case
  [c] -> c
  cs -> error $ "Unexpected number of common items: " <> cs

-- >>> map getOneWrong example
-- "pLPvts"
getOneWrong :: (String, String) -> Char
getOneWrong (l, r) = getOne . Set.toList $ Set.fromList l `Set.intersection` Set.fromList r

-- >>> solve2 example
-- 70
solve2 :: [(String, String)] -> Int
solve2 = sum . map (priority . getOneCommon) . chunksOf 3

-- >>> map getOneCommon $ chunksOf 3 example
-- "rZ"
getOneCommon :: [(String, String)] -> Char
getOneCommon = getOne . Set.toList . foldl1' Set.intersection . map (Set.fromList . uncurry (++))

example :: [(String, String)]
example = parse . unlines $
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]
