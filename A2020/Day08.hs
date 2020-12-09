{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Day08
-- Description : Solution to AOC 2020 Day 8: Handheld Halting.
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/8>
module Day08 where

import Data.Either (lefts)
import Data.Function ((&))
import Data.IntSet (IntSet, singleton, member, insert)
import Data.Vector (Vector, ifoldr, fromList, (//), (!))

-- | Solution to Day 8.
main08 :: FilePath -> IO ()
main08 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

data Ins = NoOperation Int | Jump Int | Accumulate Int deriving (Eq, Show)

parse :: String -> Vector Ins
parse = fromList . map (parseIns . words) . lines

parseIns :: [String] -> Ins
parseIns [ins, offset] =
  read (dropWhile (== '+') offset)
    & case ins of
      "acc" -> Accumulate
      "jmp" -> Jump
      "nop" -> NoOperation
      _ -> error $ "Invalid instruction " <> ins
parseIns w = error $ "Not instructions: " <> unwords w

-- >>> solve1 $ parse example
-- 5
solve1 :: Vector Ins -> Int
solve1 = report . execute
  where
    report = either (error . ("Index outside bounds: " <>) . show) id

execute :: Vector Ins -> Either Int Int
execute = execute' (singleton 0) . (,0,0)

execute' :: IntSet -> (Vector Ins, Int, Int) -> Either Int Int
execute' is state
  | pos < 0 || pos >= length v = Left acc
  | otherwise =
    if pos `member` is
      then Right acc
      else execute' nis (v, acc, pos)
  where
    (v, acc, pos) = step state
    nis = insert pos is

step :: (Vector Ins, Int, Int) -> (Vector Ins, Int, Int)
step state@(v, acc, pos) = case v ! pos of
  Accumulate a -> (v, acc + a, pos + 1)
  NoOperation _ -> succ <$> state
  Jump i -> (i +) <$> state

-- >>> solve2 $ parse example
-- 8
solve2 :: Vector Ins -> Int
solve2 = head . lefts . map execute . speculate

speculate :: Vector Ins -> [Vector Ins]
speculate v = ifoldr app [] v
  where
    app :: Int -> Ins -> [Vector Ins] -> [Vector Ins]
    app i ins = maybe id ((:) . (v //) . (: []) . (i,)) (spec ins)
    spec :: Ins -> Maybe Ins
    spec = \case
      Accumulate _ -> Nothing
      Jump i -> Just $ NoOperation i
      NoOperation i -> Just $ Jump i

example :: String
example =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]