-- |
-- Module      : Day08
-- Description : Solution to AOC 2021 Day 08: Seven Segment Search
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/08>
module Day08 where

import Data.Bifunctor (bimap)
import Data.List (partition, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set

-- | Solution to Day 08.
main08 :: FilePath -> IO ()
main08 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

-- | The display turns on seven segments named a through g.
--
-- >    0:      1:      2:      3:      4:
-- >   aaaa    ....    aaaa    aaaa    ....
-- >  b    c  .    c  .    c  .    c  b    c
-- >  b    c  .    c  .    c  .    c  b    c
-- >   ....    ....    dddd    dddd    dddd
-- >  e    f  .    f  e    .  .    f  .    f
-- >  e    f  .    f  e    .  .    f  .    f
-- >   gggg    ....    gggg    gggg    ....
-- >
-- >    5:      6:      7:      8:      9:
-- >   aaaa    aaaa    aaaa    aaaa    aaaa
-- >  b    .  b    .  .    c  b    c  b    c
-- >  b    .  b    .  .    c  b    c  b    c
-- >   dddd    dddd    ....    dddd    dddd
-- >  .    f  e    f  .    f  e    f  .    f
-- >  .    f  e    f  .    f  e    f  .    f
-- >   gggg    gggg    ....    gggg    gggg
-- >
type Digit = Set Segment

type Segment = Char

type Note = ([Digit], [Digit])

parse :: String -> [Note]
parse = map parseNote . lines

parseNote :: String -> Note
parseNote = bimap toDs toDs . splitNote
 where
  splitNote = fmap (drop 1) . break (== "|") . words
  toDs = map Set.fromList

is1478 :: Digit -> Bool
is1478 = flip elem [2, 4, 3, 7] . length

-- >>> solve1 example
-- 26
solve1 :: [Note] -> Int
solve1 = count1478 . concatMap snd
 where
  count1478 = length . filter is1478

-- | Figure displayed number based on size (after removing segments of other).
-- >>> deduce (fst example5353) Map.! (Set.fromList "abcdf")
-- 3
deduce :: [Digit] -> Map Digit Int
deduce ps = Map.fromList $ zip [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9] [0 ..]
 where
  r d = length . (\\ d)
  remove2 d1 d2 = sortOn (\d -> (r d1 d, r d2 d))
  -- find the known digits, size of 5 and 6
  ([i1, i7, i4, i8], (u5s, u6s)) = fmap (partition ((==5) . length)) . partition is1478 $ sortOn length ps
  -- [[0], [6], [9]] - ([1],[4]) = (4,3) (5,5) (4,2)
  (i9, i0, i6) = case remove2 i1 i4 u6s of [x9, x0, x6] -> (x9, x0, x6); _ -> error "bad [0,6,9]"
  -- [[2], [3], [5]] - ([4],[1]) = (3,4) (2,3) (2,4)
  (i3, i5, i2) = case remove2 i4 i1 u5s of [x3, x5, x2] -> (x3, x5, x2); _ -> error "bad [2,3,5]"

solveNote :: Note -> Int
solveNote (i, o) = sum $ zipWith (*) p10 (reverse os)
 where
  dm = deduce i
  os = map (dm Map.!) o
  p10 = map (10 ^) [0 :: Int ..]

-- >>> solve2 example
-- 61229
solve2 :: [Note] -> Int
solve2 = sum . map solveNote

example5353 :: Note
example5353 =
  parseNote
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

example :: [Note]
example =
  parse . unlines $
    [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
    , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
    , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
    , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
    , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
    , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
    , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
    , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
    , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ]
