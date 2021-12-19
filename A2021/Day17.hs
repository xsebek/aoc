-- |
-- Module      : Day17
-- Description : Solution to AOC 2021 Day 17: Trick Shot
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/17>
module Day17 where

import Linear (V2 (..))
import Optics
import Parser

-- | Solution to Day 17.
main17 :: FilePath -> IO ()
main17 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

type P2 = V2 Int

type Range = (Int, Int)
data Rectangle = R {xrange :: Range, yrange :: Range}
  deriving (Eq, Ord, Show)

_xrange :: Lens' Rectangle Range
_xrange = lens xrange (\rect xr -> rect{xrange = xr})

_yrange :: Lens' Rectangle Range
_yrange = lens yrange (\rect yr -> rect{yrange = yr})

parser :: Parser Rectangle
parser = do
  _ <- string "target area: "
  xrange <- parseRange 'x' <* string ", "
  yrange <- parseRange 'y' <* space
  pure $ R{..}
 where
  parseRange :: Char -> Parser Range
  parseRange c = range <$> (string (c <| "=") *> num <* string "..") <*> num
  num :: Parser Int
  num = signed mempty decimal
  range :: Int -> Int -> Range
  range a b = (min a b, max a b)

step :: (P2, P2) -> (P2, P2)
step (speed@(V2 x y), pos) = (V2 (max 0 $ x - 1) (y - 1), pos + speed)

-- >>> solve1 example
-- 45
solve1 :: Rectangle -> Int
solve1 s = sum [0 .. - (s ^. _yrange % _1) - 1]

inside :: P2 -> Rectangle -> Bool
inside (V2 x y) R{..} = inRange x xrange && inRange y yrange
 where
  inRange i (l, r) = min l r <= i && i <= max l r

rectangleToList :: Rectangle -> [P2]
rectangleToList (R (xmin, xmax) (ymin, ymax)) = [V2 x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]

shotRectangle :: Rectangle -> Rectangle
shotRectangle (R (_xmin, xmax) (ymin, _ymax)) = R (0, xmax) (ymin, - ymin + 10)

isGoodShot :: Rectangle -> P2 -> Bool
isGoodShot rect p = any (`inside` rect) . takeWhile (`inside` rect2) . map snd $ iterate step (p, V2 0 0)
 where
  rect2 = shotRectangle rect & _yrange % _2 .~ solve1 rect

-- >>> solve2 example
-- 112
solve2 :: Rectangle -> Int
solve2 r = length . filter (isGoodShot r) . rectangleToList $ shotRectangle r

-- >>> parseExample' parser "target area: x=124..174, y=-123..-86"
-- R {xrange = (124,174), yrange = (-123,-86)}
example :: Rectangle
example = parseExample' parser "target area: x=20..30, y=-10..-5"
