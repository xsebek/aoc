-- |
-- Module      : Day18
-- Description : Solution to AOC 2021 Day 18: Snailfish
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/18>
module Day18 where

import Control.Applicative ((<|>))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (foldl1')
import Data.Text (Text)
import Data.Text qualified as T
import Optics
import Parser

-- | Solution to Day 18.
main18 :: FilePath -> IO ()
main18 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

data Pair a = R {getR :: a} | P {fstP :: Pair a, sndP :: Pair a}
  deriving (Eq, Ord, Show, Functor)

-- | Snailfish integer.
type SInt = Pair Int

parser :: Parser [SInt]
parser = parseSnailInt `sepEndBy` eol
 where
  parseSnailInt = (R <$> decimal) <|> bracket (P <$> parseSnailInt <*> (char ',' *> parseSnailInt))
  bracket p = char '[' *> p <* char ']'

-- >>> pretty . foldl1' (<>) $ exampleN 4
-- "[[[[1,1],[2,2]],[3,3]],[4,4]]"
-- >>> pretty . foldl1' (<>) $ exampleN 5
-- "[[[[3,0],[5,3]],[4,4]],[5,5]]"
-- >>> pretty . foldl1' (<>) $ exampleN 6
-- "[[[[5,0],[7,4]],[5,5]],[6,6]]"
instance Semigroup SInt where
  l <> r = reduce $ P l r

reduce :: SInt -> SInt
reduce s
  | exploded = reduce se
  | splitted = reduce ss
  | otherwise = s
 where
  (exploded, _ml, _mr, se) = explode 0 s
  (splitted, ss) = split se

-- | Explode leftmost pair that is 4+ pairs deep.
explode :: Int -> SInt -> (Bool, Maybe Int, Maybe Int, SInt)
explode layer p = case p of
  R n -> (False, Nothing, Nothing, R n)
  P p1 p2 ->
    if layer >= 4
      then case p of
        -- EXPLODE PAIR
        P (R l) (R r) -> (True, Just l, Just r, R 0)
        -- EXPLODE INSIDE RIGHT
        P (R _) _p2 -> explode (succ layer) p2 & _4 %~ P p1
        -- EXPLODE INSIDE LEFT
        P __p1 __p2 -> explode (succ layer) p1 & _4 %~ (`P` p2)
      else case mr1 of
        -- ADD TO LEFTMOST IN RIGHT & BUBBLE UP
        Just r1 -> (True, ml1, Nothing, P s1 (addL r1 p2))
        -- CONTINUE TO RIGHT SUBPAIR
        Nothing ->
          if b1
            then -- STOP AND BUBBLE UP
              (True, ml1, Nothing, P s1 p2)
            else case ml2 of
              -- ADD TO RIGHTMOST IN LEFT & BUBBLE UP
              Just l2 -> (True, ml1, mr2, P (addR l2 s1) s2)
              -- BUBBLE UP
              Nothing -> (b1 || b2, ml1, mr2, P s1 s2)
 where
  (b1, ml1, mr1, s1) = explode (succ layer) (fstP p)
  (b2, ml2, mr2, s2) = explode (succ layer) (sndP p)

-- | Split leftmost regular number that is greater than 9.
split :: SInt -> (Bool, SInt)
split p = case p of
  R n ->
    if n < 10
      then (False, R n)
      else (True, P (R $ n `div` 2) (R $ n `div` 2 + n `mod` 2))
  P p1 p2 ->
    let (b1, s1) = split p1
        (b2, s2) = split p2
     in if b1 then (True, P s1 p2) else (b2, P s1 s2)

addR :: Int -> SInt -> SInt
addR i = \case
  R n -> R $ i + n
  P pl pr -> P pl (addR i pr)

addL :: Int -> SInt -> SInt
addL i = \case
  R n -> R $ i + n
  P pl pr -> P (addL i pl) pr

cataP :: (a -> b) -> (b -> b -> b) -> Pair a -> b
cataP f g = \case
  R a -> f a
  P pl pr -> g (cataP f g pl) (cataP f g pr)

-- >>> magnitude `onExample` "[[1,2],[[3,4],5]]"
-- [143]
magnitude :: SInt -> Int
magnitude = cataP id (\l r -> l * 3 + r * 2)

-- >>> solve1 example2
-- 4140
solve1 :: [SInt] -> Int
solve1 = magnitude . foldl1' (<>)

largestPairBy :: (Eq a, Ord b) => (a -> a -> b) -> [a] -> ((a, a), b)
largestPairBy op xs = maximumBy (compare `on` snd) pairValues
 where
  combinations2 = filter (uncurry (/=)) [(x, y) | x <- xs, y <- xs]
  pairValues = zip combinations2 $ map (uncurry op) combinations2

-- >>> over _1 (over both pretty) $ maxAddMagnitude example2
-- (("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]","[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"),3416)
maxAddMagnitude :: [SInt] -> ((SInt, SInt), Int)
maxAddMagnitude = largestPairBy (\l r -> magnitude $ l <> r)

-- >>> solve2 example2
-- 3993
solve2 :: [SInt] -> Int
solve2 = snd . maxAddMagnitude

pretty :: Show a => Pair a -> String
pretty = \case
  R a -> show a
  P p1 p2 -> '[' : pretty p1 <> (',' : pretty p2) <> "]"

onExamplePretty :: (SInt -> SInt) -> Text -> IO ()
onExamplePretty f t = mapM_ (putStrLn . pretty) $ f `onExample` t

onExample :: (SInt -> b) -> Text -> [b]
onExample f t = f <$> parseExample' parser t

exampleN :: Int -> [SInt]
exampleN n = let xn = map R [1 .. n] in zipWith P xn xn

example1 :: [SInt]
example1 =
  parseExample' parser . T.unlines $
    [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
    , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
    , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
    , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
    , "[7,[5,[[3,8],[1,4]]]]"
    , "[[2,[2,2]],[8,[8,1]]]"
    , "[2,9]"
    , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
    , "[[[5,[7,4]],7],1]"
    , "[[[[4,2],2],6],[8,7]]"
    ]

example2 :: [SInt]
example2 =
  parseExample' parser . T.unlines $
    [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
    , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
    , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
    , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
    , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
    , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
    , "[[[[5,4],[7,7]],8],[[8,3],8]]"
    , "[[9,3],[[9,9],[6,[4,9]]]]"
    , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
    , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    ]
