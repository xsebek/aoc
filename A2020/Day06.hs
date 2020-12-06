module Day06 where

import Control.Monad ((>=>))
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Solution

solution :: Solve [[Text]]
solution = solveG (TextIO.readFile >=> pure . parse) solve1 solve2

-- >>> parse example
-- [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]]
parse :: Text -> [[Text]]
parse = map Text.lines . splitOn "\n\n"

-- >>> solve1 $ parse example
-- 11
solve1 :: [[Text]] -> Int
solve1 = sum . map (length . groupSet)

-- >>> groupSet exampleGroup
-- fromList "abcxyz"
groupSet :: [Text] -> Set Char
groupSet = foldMap (Set.fromList . Text.unpack)

-- >>> solve2 $ parse example
-- 6
solve2 :: [[Text]] -> Int
solve2 = sum . map (length . groupIntersect)

-- >>> groupIntersect exampleGroup
-- fromList "abc"
groupIntersect :: [Text] -> Set Char
groupIntersect = foldr1 (flip intersection) . map (Set.fromList . Text.unpack)

example :: Text
example = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"

exampleGroup :: [Text]
exampleGroup =
  [ "abcx",
    "abcy",
    "abcz"
  ]