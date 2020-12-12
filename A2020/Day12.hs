{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoOverloadedStrings #-}

-- |
-- Module      : Day12
-- Description : Solution to AOC 2020 Day 12: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/12>
module Day12 where

import Data.List (foldl')
import Data.Maybe
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as MCL

-- | Solution to Day 12.
main12 :: FilePath -> IO ()
main12 f = do
  input <- parseActions =<< readFile f
  print $ solve1 input
  print $ solve2 input

data Action = Move (Maybe Dir) Int | Turn (Either Int Int) deriving (Eq)

data Dir = E | N | W | S deriving (Eq, Show, Read, Enum)

type Parser = Parsec Void String

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
  act <$> MCL.decimal
  where
    moving = Move . Just . read . (: []) <$> oneOf "NSEW" <?> "Ship direction"
    forward = Move Nothing <$ string "F" <?> "Move forward"
    turning = (\c -> Turn . readE c) <$> oneOf "LR" <?> "Turn ship Left/Right."
    readE = \case
      'L' -> Left
      'R' -> Right
      _ -> error "not L/R turning"

data Ship = Ship {dir :: Dir, posX :: Int, posY :: Int} deriving (Eq, Show)

ship :: Ship
ship = Ship E 0 0

-- >>> solve1 <$> parseActions example
-- 25
solve1 :: [Action] -> Int
solve1 = (\s -> abs (posX s) + abs (posY s)) . navigate ship

-- >>> navigate ship <$> parseActions example
-- Ship {dir = S, posX = 17, posY = -8}
navigate :: Ship -> [Action] -> Ship
navigate = foldl' (flip captain)

-- >>> captain (Move Nothing 10) ship
-- Ship {dir = E, posX = 0, posY = 10}
-- >>> captain (Turn (Right 90)) (Ship {dir = E, posX = 17, posY = 3})
-- Ship {dir = S, posX = 17, posY = 3}
captain :: Action -> Ship -> Ship
captain a = case a of
  Move md i -> move i md
  Turn e -> either turn (turn . negate) e

-- >>> turn (Ship {dir = E, posX = 17, posY = 3}) (-90)
-- Ship {dir = S, posX = 17, posY = 3}
turn :: Int -> Ship -> Ship
turn a s = s {dir = dir s + fromAngle a}

fromAngle :: Enum a => Int -> a
fromAngle a = toEnum $ (a `div` 90) `mod` 4

-- >>> move 10 Nothing ship
-- Ship {dir = E, posX = 10, posY = 0}
move :: Int -> Maybe Dir -> Ship -> Ship
move i md s = case d of
  N -> s {posY = posY s + i}
  S -> s {posY = posY s - i}
  E -> s {posX = posX s + i}
  W -> s {posX = posX s - i}
  where
    d = fromMaybe (dir s) md

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

example :: String
example = "F10\nN3\nF7\nR90\nF11\n"

instance Show Action where
  show (Move Nothing i) = "Move F " <> show i
  show (Move (Just d) i) = "Move " <> show d <> " " <> show i
  show (Turn e) = "Turn " <> show e

-- >>> (liftEnumMod 4 negate) S
-- N
liftEnumMod :: Enum a => Int -> (Int -> Int) -> a -> a
liftEnumMod m f i = toEnum $ f (fromEnum i) `mod` m

-- >>> (liftEnum2Mod 4 (+))
liftEnum2Mod :: Enum a => Int -> (Int -> Int -> Int) -> a -> a -> a
liftEnum2Mod m f i j = toEnum $ (fromEnum i `f` fromEnum j) `mod` m

instance Num Dir where
  fromInteger = fromIntegral . fromEnum
  (+) = liftEnum2Mod 4 (+)
  (-) = liftEnum2Mod 4 (-)
  (*) = liftEnum2Mod 4 (*)
  abs = liftEnumMod 4 abs
  signum = liftEnumMod 4 signum
  negate = liftEnumMod 4 negate