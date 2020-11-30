{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

-- | Solution to AOC 2015 Day 6: Probably a Fire Hazard
--
-- https://adventofcode.com/2015/day/6
--
-- Because your neighbors keep defeating you in the holiday house decorating
-- contest year after year, you've decided to deploy one million lights in
-- a 1000x1000 grid.
--
-- Furthermore, because you've been especially nice this year, Santa has mailed
-- you instructions on how to display the ideal lighting configuration.
--
-- Lights in your grid are numbered from 0 to 999 in each direction;
-- the lights at each corner are at 0,0, 0,999, 999,999, and 999,0.
-- The instructions include whether to turn on, turn off, or toggle various
-- inclusive ranges given as coordinate pairs. Each coordinate pair represents
-- opposite corners of a rectangle, inclusive; a coordinate pair like 0,0
-- through 2,2 therefore refers to 9 lights in a 3x3 square.
--
-- The lights all start turned off.
--
-- To defeat your neighbors this year, all you have to do is set up your lights
-- by doing the instructions Santa sent you in order.
module Day06 where

type P = (Int, Int)

data Instruction = On P P | Off P P | Toggle P P deriving (Eq, Show)

get :: Instruction -> (P, P)
get (On l r) = (l, r)
get (Off l r) = (l, r)
get (Toggle l r) = (l, r)

-- | Process input text file.
main :: IO ()
main = do
  input <- lines <$> readFile "input06.txt"
  let instructions = map instruction input
  -- mapM_ print instructions
  print (solve1 instructions)
  print (solve2 instructions)

-- | Part One.
--
-- >>> solve1 [Toggle (0,0) (999,0)]
-- 1000
-- >>> solve1 [On (0,0) (999,999), Off (499,499) (500,500)]
-- 999996
solve1 :: [Instruction] -> Int
solve1 ins = length . filter id . map (`isOn1` reverse ins) $ million
  where
    isOn1 = (`isOn` False)

million :: [P]
million = [(x, y) | x <- [0 .. 999], y <- [0 .. 999]]

instruction :: String -> Instruction
instruction s = case words s of
  ["turn", "on", l, "through", r] -> On (p l) (p r)
  ["turn", "off", l, "through", r] -> Off (p l) (p r)
  ["toggle", l, "through", r] -> Toggle (p l) (p r)
  line -> error $ "Could not parse line: " ++ unwords line
  where
    p :: String -> P
    p s = read $ '(' : s ++ ")"

-- | Check *reverse* instructions if the light is on.
isOn :: P -> Bool -> [Instruction] -> Bool
isOn _ _ [] = False
isOn p state (i : ins) =
  if not (pointIn i)
    then rest
    else case i of
      On _ _ -> True
      Off _ _ -> False
      Toggle _ _ -> not rest
  where
    pointIn = lightIn p
    rest = isOn p state ins

lightIn :: P -> Instruction -> Bool
lightIn (x, y) = pointIn . get
  where
    pointIn ((lx, ly), (rx, ry)) = inside lx x rx && inside ly y ry
    inside l x r = l <= x && x <= r

-- | Part Two.
--
-- >>> solve2 [On (0,0) (0,0), On (0,0) (0,0), Off (0,0) (0,0)]
-- 1
-- >>> solve2 [Toggle (0,0) (999,999)]
-- 2000000
solve2 :: [Instruction] -> Int
solve2 ins = sum . map (\p -> foldr (much p) 0 $ reverse ins) $ million
  where
    min0Add i x = max 0 $ i + x
    much :: P -> Instruction -> Int -> Int
    much p i = if p `lightIn` i then min0Add $ muchOn i else id

muchOn :: Instruction -> Int
muchOn = \case
  On _ _ -> 1
  Off _ _ -> -1
  Toggle _ _ -> 2
