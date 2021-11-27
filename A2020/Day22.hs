-- |
-- Module      : Day22
-- Description : Solution to AOC 2020 Day 22: Crab Combat
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/22>
module Day22 where

import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Foldable (find, Foldable (toList))
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text, breakOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)

-- | Solution to Day 22.
-- >>> main22 "input/2020/22.txt"
main22 :: FilePath -> IO ()
main22 f = do
  input <- parse <$> T.readFile f
  print $ solve1 input
  print $ solve2 input

data Player = P
  { name :: Text
  , cards :: Seq Int
  }
  deriving (Show, Eq)

parse :: Text -> (Player, Player)
parse = bimap toPlayer toPlayer . splitPlayers

toPlayer :: Text -> Player
toPlayer = uncurry P . nameCards . T.lines
 where
  nameCards :: [Text] -> (Text, Seq Int)
  nameCards (x : xs) = (T.init x, toCards $ map (readMaybe . T.unpack) xs)
  nameCards _ = error "Player must have a name!"
  toCards :: [Maybe Int] -> Seq Int
  toCards = Seq.fromList . map (expectJust "Card must be an number!")
  expectJust :: String -> Maybe a -> a
  expectJust _ (Just a) = a
  expectJust er Nothing = error er

splitPlayers :: Text -> (Text, Text)
splitPlayers = second (T.drop 2) . breakOn "\n\n"

play :: (Player, Player) -> (Player, Player)
play (p1@(P n1 (c1 :<| cs1)), p2@(P n2 (c2 :<| cs2))) = case c1 `compare` c2 of
  EQ -> (p1, p2)
  LT -> (P n1 cs1, P n2 (cs2 |> c2 |> c1))
  GT -> (P n1 (cs1 |> c1 |> c2), P n2 cs2)
play ps = ps

-- >>> solve1 exampleIn
-- 306
solve1 :: (Player, Player) -> Int
solve1 = winScore . fromJust . find anyWin . iterate play
 where
  anyWin = uncurry (||) . both (null . cards)
  winScore = uncurry max . both (sum . zipWith (*) [1..] . toList . Seq.reverse . cards)

-- >>> solve2 exampleIn
solve2 :: a -> Int
solve2 = undefined

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- >>> exampleIn
-- (P {name = "Player 1", card = fromList [9,2,6,3,1]},P {name = "Player 2", card = fromList [5,8,4,7,10]})
exampleIn :: (Player, Player)
exampleIn = parse example

example :: Text
example =
  T.pack . unlines $
    [ "Player 1:"
    , "9"
    , "2"
    , "6"
    , "3"
    , "1"
    , ""
    , "Player 2:"
    , "5"
    , "8"
    , "4"
    , "7"
    , "10"
    ]
