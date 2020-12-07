-- |
-- Module      : Day06
-- Description : Solution to AOC 2015 Day 6: Custom Customs.
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/6>
module Day06 where

import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

-- | Solution to Day 6.
--
-- As your flight approaches the regional airport where you\'ll switch to a
-- much larger plane,
-- <https://en.wikipedia.org/wiki/Customs_declaration customs declaration forms>
-- are distributed to the passengers.
--
-- The form asks a series of 26 yes-or-no questions marked @a@ through @z@.
-- All you need to do is identify the questions for which
-- /anyone in your group/ answers "yes". Since your group is just you,
-- this doesn\'t take very long.
--
-- However, the person sitting next to you seems to be experiencing a
-- language barrier and asks if you can help. For each of the people in
-- their group, you write down the questions for which they answer \"yes\",
-- one per line. For example:
--
-- > abcx
-- > abcy
-- > abcz
--
-- In this group, there are /@6@/ questions to which anyone answered
-- \"yes\": @a@, @b@, @c@, @x@, @y@, and @z@. (Duplicate answers to the
-- same question don\'t count extra; each question counts at most once.)
--
-- Another group asks for your help, then another, and eventually you\'ve
-- collected answers from every group on the plane (your puzzle input).
main06 :: FilePath -> IO ()
main06 f = do
  input <- parse <$> TextIO.readFile f
  print $ solve1 input
  print $ solve2 input

-- Each group\'s answers are separated by a blank line, and within each
-- group, each person\'s answers are on a single line. For example:
--
-- > abc
-- >
-- > a
-- > b
-- > c
-- >
-- > ab
-- > ac
-- >
-- > a
-- > a
-- > a
-- > a
-- >
-- > b
--
-- This list represents answers from five groups:
--
-- -   The first group contains one person who answered \"yes\" to /@3@/
--     questions: @a@, @b@, and @c@.
-- -   The second group contains three people; combined, they answered
--     \"yes\" to /@3@/ questions: @a@, @b@, and @c@.
-- -   The third group contains two people; combined, they answered \"yes\"
--     to /@3@/ questions: @a@, @b@, and @c@.
-- -   The fourth group contains four people; combined, they answered
--     \"yes\" to only /@1@/ question, @a@.
-- -   The last group contains one person who answered \"yes\" to only
--     /@1@/ question, @b@.
--
-- >>> parse example
-- [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]]
parse :: Text -> [[String]]
parse = map (lines . Text.unpack) . splitOn "\n\n"

-- In the example, the sum of the counts is @3 + 3 + 3 + 1 + 1@ = /@11@/.
--
-- For each group, count the number of questions to which anyone answered
-- \"yes\". /What is the sum of those counts?/
--
-- >>> solve1 $ parse example
-- 11
solve1 :: [[String]] -> Int
solve1 = sum . map (length . groupSet)

-- >>> groupSet exampleGroup
-- fromList "abcxyz"
groupSet :: [String] -> Set Char
groupSet = foldMap Set.fromList

-- | Part Two.
--
-- As you finish the last group\'s customs declaration, you notice that you
-- misread one word in the instructions:
--
-- You don\'t need to identify the questions to which /anyone/ answered
-- \"yes\"; you need to identify the questions to which /everyone/ answered
-- \"yes\"!
--
-- So in the example, the real sum of the counts is @3 + 0 + 1 + 1 + 1@ = /@6@/.
--
-- For each group, count the number of questions to which /everyone/
-- answered \"yes\". /What is the sum of those counts?/
--
-- >>> solve2 $ parse example
-- 6
solve2 :: [[String]] -> Int
solve2 = sum . map (length . groupIntersect)

-- >>> groupIntersect exampleGroup
-- fromList "abc"
groupIntersect :: [String] -> Set Char
groupIntersect = foldr1 (flip intersection) . map Set.fromList

example :: Text
example = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"

exampleGroup :: [String]
exampleGroup =
  [ "abcx",
    "abcy",
    "abcz"
  ]