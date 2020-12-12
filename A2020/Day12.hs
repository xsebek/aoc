-- |
-- Module      : Day12
-- Description : Solution to AOC 2020 Day 12: Rain Risk
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/12>
module Day12 where

import Control.Arrow ((***))
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Optics.Core (Lens, lens, over', (%!~))
import Text.Megaparsec
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- | Solution to Day 12.
main12 :: FilePath -> IO ()
main12 f = do
  input <- parseActions =<< readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "12"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

data Action = Move (Maybe Dir) Int | Turn (Either Int Int) deriving (Eq)

data Dir = E | N | W | S deriving (Eq, Show, Read, Enum)

type Parser = Parsec Void String

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

-- >>> parseActions example
-- [Move F 10,Move N 3,Move F 7,Turn Right 90,Move F 11]
parseActions :: MonadFail m => String -> m [Action]
parseActions = eToM . runParser parsing "Day 12"
  where
    parsing = parseAction `endBy` eol
    eToM = either (fail . errorBundlePretty) pure

parseAction :: Parser Action
parseAction = do
  act <- moving <|> turning <|> forward
  act <$> decimal
  where
    forward = Move Nothing <$ string "F" <?> "Move forward"
    moving =
      Move . Just . read . (: [])
        <$> oneOf ['N', 'S', 'E', 'W']
        <?> "Ship direction"
    turning =
      (\c -> Turn . readE c)
        <$> oneOf ['L', 'R']
        <?> "Turn ship Left/Right."
    readE = \case
      'L' -> Left
      'R' -> Right
      _ -> error "not L/R turning"

data Ship = Ship {dir :: Dir, pos :: Point, way :: Point} deriving (Eq, Show)

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

pointToPair :: Point -> (Int, Int)
pointToPair (Point x y) = (x, y)

ship :: Ship
ship = Ship E (Point 0 0) waypoint

waypoint :: Point
waypoint = Point 10 1

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 <$> parseActions example
-- 25
solve1 :: [Action] -> Int
solve1 = manhattan . navigate captain ship

manhattan :: Ship -> Int
manhattan = uncurry (+) . (abs *** abs) . pointToPair . pos

type Captain = Action -> Ship -> Ship

-- >>> navigate ship <$> parseActions example
-- Ship {dir = S, x = 17, y = -8}
navigate :: Captain -> Ship -> [Action] -> Ship
navigate = foldl' . flip

-- >>> captain (Move Nothing 10) ship
-- Ship {dir = E, x = 0, y = 10}
-- >>> captain (Turn (Right 90)) (Ship {dir = E, x = 17, y = 3})
-- Ship {dir = S, x = 17, y = 3}
captain :: Action -> Ship -> Ship
captain = \case
  Move md i -> moveShip i md
  Turn e -> either turn (turn . negate) e

-- >>> turn (-90) (Ship E (Point 17 3) waypoint)
-- Ship {dir = S, pos = Point {x = 17, y = 3}, way = Point {x = 10, y = 1}}
turn :: Int -> Ship -> Ship
turn a s = s {dir = dir s `rotateAngle` a}

rotateAngle :: Dir -> Int -> Dir
rotateAngle d a
  | a `mod` 90 == 0 = toEnum $ (fromEnum d + a `div` 90) `mod` 4
  | otherwise = error $ "The angle " <> show a <> " is not allowed!"

-- >>> moveShip 10 Nothing ship
-- Ship {dir = E, pos = Point {x = 10, y = 0}, way = Point {x = 10, y = 1}}
moveShip :: Int -> Maybe Dir -> Ship -> Ship
moveShip i md s = over' _pos (movePoint i d) s
  where
    d = dir s `fromMaybe` md

movePoint :: Int -> Dir -> Point -> Point
movePoint i = \case
  N -> _y %!~ (+ i)
  S -> _y %!~ (- i +)
  E -> _x %!~ (+ i)
  W -> _x %!~ (- i +)

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 <$> parseActions example
-- 286
solve2 :: [Action] -> Int
solve2 = manhattan . navigate captain2 ship

-- >>> navigate captain2 ship <$> parseActions example
-- Ship {dir = E, pos = Point {x = 214, y = -72}, way = Point {x = 4, y = -10}}
captain2 :: Action -> Ship -> Ship
captain2 = \case
  Move Nothing i -> moveWay i
  Move (Just d) i -> over' _way (movePoint i d)
  Turn e -> either (rotateWay . negate) rotateWay e

rotateWay :: Int -> Ship -> Ship
rotateWay i = over' _way $ rotatePoint i

moveWay :: Int -> Ship -> Ship
moveWay i s = over' _pos (addP i $ way s) s
  where
    addP i (Point lx ly) (Point rx ry) = Point (i * lx + rx) (i * ly + ry)

-- >>> rotatePoint 270 (Point 1 1)
-- Point {x = -1, y = 1}
rotatePoint :: Int -> Point -> Point
rotatePoint i p@(Point x y) = case rotateAngle E i of
  E -> p
  N -> Point y (- x)
  W -> Point (- x) (- y)
  S -> Point (- y) x

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

example :: String
example = "F10\nN3\nF7\nR90\nF11\n"

--------------------------------------------------------------------
--                             MISC                               --
--------------------------------------------------------------------

instance Show Action where
  show (Move Nothing i) = 'F' : show i
  show (Move (Just d) i) = show d <> show i
  show (Turn e) = either (('L' :) . show) (('R' :) . show) e

_dir :: Lens Ship Ship Dir Dir
_dir = lens dir (\s d -> s {dir = d})

_way :: Lens Ship Ship Point Point
_way = lens way (\s d -> s {way = d})

_pos :: Lens Ship Ship Point Point
_pos = lens pos (\s p -> s {pos = p})

_x :: Lens Point Point Int Int
_x = lens x (\p i -> p {x = i})

-- >>> _y %!~ (+1) $ (Point 0 0)
-- Point {x = 0, y = 1}
_y :: Lens Point Point Int Int
_y = lens y (\p i -> p {y = i})