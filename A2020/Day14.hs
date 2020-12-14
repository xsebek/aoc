-- |
-- Module      : Day14
-- Description : Solution to AOC 2020 Day 14: Docking Data
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/14>
module Day14 where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits
import qualified Data.IntMap.Strict as Map
import Data.List (foldl')
import Data.Void (Void)
import Data.Word (Word64)
import Numeric.Optics (binary)
import Optics.Core (ReversibleOptic (re), view)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Value = Word64

type Memory = Map.IntMap Value

-- | Mask for part 1.
type Mask = (Value, Value)

data In = M String | S Value Value deriving (Eq, Show)

type Input = [In]

-- | Solution to Day 14.
main14 :: FilePath -> IO ()
main14 f = do
  input <- parse =<< readFile f
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bgroup
        "14"
        [ bench "part1" $ whnf solve1 input,
          bench "part2" $ whnf solve2 input
        ]
    ]

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

type Parser = Parsec Void String

-- >>> parse example :: IO Input
-- [M (137438953469,64),S 8 11,S 7 101,S 8 0]
parse :: MonadFail m => String -> m Input
parse = eToM . runParser ((mask <|> mem) `endBy` eol) "Day14"
  where
    eToM = either (fail . errorBundlePretty) pure
    mask :: Parser In
    mask = M <$> (string "mask = " *> count 36 (oneOf ['0', '1', 'X']))
    mem :: Parser In
    mem = S <$> (string "mem[" *> decimal) <*> (string "] = " *> decimal)

-- >>> maskBin startMask
-- ("1111111111111111111111111111111111111","0")
startMask :: Mask
startMask = (2 ^ 37 - 1, 0)

-- | Create a mask, where first is the 1s with masking 0 and second vice versa.
--
-- >>> maskBin $ createMask exampleMask
-- ("1111111111111111111111111111111111101","1000000")
createMask :: String -> Mask
createMask = foldl' go startMask . zip [35, 34 .. 0]
  where
    go :: (Value, Value) -> (Int, Char) -> (Value, Value)
    go m@(mzeros, mones) (n, c) = case c of
      '0' -> (,mones) $ clearBit mzeros n
      '1' -> (mzeros,) $ setBit mones n
      _ -> m

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 <$> parse example
-- 165
solve1 :: Input -> Value
solve1 = sum . Map.elems . snd . runPart1

runPart1 :: Input -> (Mask, Memory)
runPart1 = foldl' evaluate (startMask, Map.empty)

-- >>> mask 11 $ createMask exampleMask
-- 73
mask :: Value -> Mask -> Value
mask v (mzeros, mones) = (v .&. mzeros) .|. mones

evaluate :: (Mask, Memory) -> In -> (Mask, Memory)
evaluate (ms, mem) = \case
  M newMask -> (createMask newMask, mem)
  S index value -> (ms,) $ Map.insert (fromIntegral index) (mask value ms) mem

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 <$> parse example2
-- 208
solve2 :: Input -> Value
solve2 = sum . Map.elems . snd . runPart2

runPart2 :: Input -> (String, Memory)
runPart2 = foldl' evaluate2 (replicate 36 '0', Map.empty)

-- >>> overwrite 42 "000000000000000000000000000000X1001X"
-- [58,26,59,27]
overwrite :: Value -> String -> [Value]
overwrite v = foldl' (flip go) [v] . zip [35, 34 .. 0]
  where
    go :: (Int, Char) -> [Value] -> [Value]
    go = \case
      (_, '0') -> id
      (n, '1') -> map (`setBit` n)
      (n, _) -> concatMap (\v -> [v, complementBit v n])

-- >>> evaluate2 ("000000000000000000000000000000X1001X", Map.empty) (S 42 100)
-- ("000000000000000000000000000000X1001X",fromList [(26,100),(27,100),(58,100),(59,100)])
evaluate2 :: (String, Memory) -> In -> (String, Memory)
evaluate2 (ms, mem) = \case
  M newMask -> (newMask, mem)
  S index value -> (ms,) $ foldl' (updateMap value) mem (overwrite index ms)
  where
    updateMap v m i = Map.insert (fromIntegral i) v m

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

exampleMask :: String
exampleMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

example :: String
example =
  unlines
    [ "mask = " <> exampleMask,
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    ]

example2 :: String
example2 =
  unlines
    [ "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    ]

toBin :: Value -> String
toBin = view (re binary)

maskBin :: Mask -> (String, String)
maskBin = bimap toBin toBin