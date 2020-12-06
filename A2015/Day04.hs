{-# LANGUAGE OverloadedStrings #-}

-- | Day 4: The Ideal Stocking Stuffer.
--
-- https://adventofcode.com/2015/day/4
--
-- Santa needs help mining some AdventCoins (very similar to bitcoins)
-- to use as gifts for all the economically forward-thinking little girls
-- and boys.
--
-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with
-- at least five zeroes. The input to the MD5 hash is some secret key (your
-- puzzle input, given below) followed by a number in decimal.
-- To mine AdventCoins, you must find Santa the lowest positive number
-- (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
--
-- >>> md5 "abcdef609043"
-- 000001dbbfa3a5c83a2d506429c7b00e
-- >>> md5 "pqrstuv1048970"
-- 000006136ef2ff3b291c85725f17325c
module Day04 where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Digest.Pure.MD5 (md5)
import Solution

solution :: Solution B.ByteString B.ByteString
solution =
  let readFrom = B.readFile
      parse = head . BC.words
      solution1 = BC.putStrLn . solve1
      solution2 = BC.putStrLn . solve2
   in Solution {..}

-- | Part One.
--
-- >>> solve1 "abcdef"
-- "abcdef609043"
-- >>> solve1 "pqrstuv"
-- "pqrstuv1048970"
solve1 :: B.ByteString -> B.ByteString
solve1 = coinByPredicate (\s -> "00000" == take 5 (hash s))

coinByPredicate :: (B.ByteString -> Bool) -> B.ByteString -> B.ByteString
coinByPredicate p secret = head $ filter p $ m <$> [0 ..]
  where
    m i = secret <> BC.pack (show i)

-- | Get the MD5 hash of input String.
--
-- >>> hash "pqrstuv1048970"
-- "000006136ef2ff3b291c85725f17325c"
hash :: B.ByteString -> String
hash = show . md5

-- | Part Two.
--
-- Now find one that starts with six zeroes.
--
-- _This is quite slow, but still finishes in less then a minute._
solve2 :: B.ByteString -> B.ByteString
solve2 = coinByPredicate (\s -> "000000" == take 6 (hash s))