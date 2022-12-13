{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Day13
-- Description : Solution to AOC 2022 Day 13: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/13>
module Day13 where
import Parser (Parser)
import qualified Parser as P
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.List as List

-- | Solution to Day 13.
main13 :: FilePath -> IO ()
main13 f = do
  input <- P.parseFile parser f
  print $ solve1 input
  print $ solve2 input

parser :: Parser [(Packet, Packet)]
parser = ((,) <$> packetParser <* P.eol <*> packetParser <* P.eol) `P.sepBy` P.eol

-- >>> P.parseExample' packetParser "[[4,4],4,4]"
-- L [L [I 4,I 4],I 4,I 4]
-- >>> P.parseExample' packetParser "[[[]]]"
-- L [L [L []]]
packetParser :: Parser Packet
packetParser = i <|> (P.char '[' *> l <* P.char ']')
 where
  i = I <$> P.decimal
  l = L <$> packetParser `P.sepBy` P.char ','

data Packet = I Int | L [Packet]
 deriving (Eq, Show)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare p1 p2 = case (p1,p2) of
    (I i, L l) -> compare (L [I i]) (L l)
    (L l, I i) -> compare (L l) (L [I i])
    (I i, I j) -> compare i j
    (L l1, L l2) -> compare l1 l2

-- >>> solve1 example
-- 13
solve1 :: [(Packet, Packet)] -> Int
solve1 = sum . map fst . filter (uncurry (<=) . snd). zip [1..]


-- >>> solve2 example
-- 140
solve2 :: [(Packet, Packet)] -> Int
solve2 = product . findDividers . List.sort . (dividers <>) . concatMap (\(a,b) -> [a,b])

findDividers :: [Packet] -> [Int]
findDividers = map fst . filter (flip elem dividers . snd) . zip [1..]

dividers :: [Packet]
dividers = [L [L [I 2]], L [L [I 6]]]

example :: [(Packet, Packet)]
example = P.parseExample' parser . T.unlines $
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , ""
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , ""
  , "[9]"
  , "[[8,7,6]]"
  , ""
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , ""
  , "[7,7,7,7]"
  , "[7,7,7]"
  , ""
  , "[]"
  , "[3]"
  , ""
  , "[[[]]]"
  , "[[]]"
  , ""
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]
