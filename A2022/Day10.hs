-- |
-- Module      : Day10
-- Description : Solution to AOC 2022 Day 10: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/10>
module Day10 where
import Data.List.Split (chunksOf)

-- | Solution to Day 10.
main10 :: FilePath -> IO ()
main10 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  mapM_ putStrLn $ solve2 input

parse :: String -> [Inst]
parse = map readInst . lines
 where
  readInst l = case words l of
    ["noop"] -> Noop
    ["addx", i] -> AddX (read i)
    other -> error $ "Could not parse instr: " <> show other

data Inst = Noop | AddX Int
  deriving (Eq, Ord, Show)

-- >>> solve1 example
-- 13140
solve1 :: [Inst] -> Int
solve1 = sum . take20to220 . zipWith (*) [1..] . evalInst 1

-- >>> take20to220 [1..]
-- [20,60,100,140,180,220]
take20to220 :: [a] -> [a]
take20to220 xs =
  let (x20, nxs) = splitAt 20 xs
   in last x20 : take 5 (map last $ chunksOf 40 nxs)

-- >>> evalInst 1 exampleSmall
-- [1,1,1,4,4,-1]
evalInst :: Int -> [Inst] -> [Int]
evalInst s = \case
  [] -> []
  (Noop:is) -> s : evalInst s is
  (AddX x: is) -> s: s: evalInst (s + x) is

-- >>> error . ("This is fine.\n" <>) . unlines $ solve2 example
-- *** Exception: This is fine.
-- ██..██..██..██..██..██..██..██..██..██..
-- ███...███...███...███...███...███...███.
-- ████....████....████....████....████....
-- █████.....█████.....█████.....█████.....
-- ██████......██████......██████......████
-- ███████.......███████.......███████.....
solve2 :: [Inst] -> [String]
solve2 = chunksOf 40 . zipWith pixel (cycle [0..39]) . evalInst 1

pixel :: Int -> Int -> Char
pixel pos sprite = if sprite - 1 <= pos && pos <= sprite + 1 then '█' else '.'

exampleSmall :: [Inst]
exampleSmall =
  [ Noop
  , AddX 3
  , AddX (-5)
  ]

example :: [Inst]
example = parse . unlines $
  [ "addx 15"
  , "addx -11"
  , "addx 6"
  , "addx -3"
  , "addx 5"
  , "addx -1"
  , "addx -8"
  , "addx 13"
  , "addx 4"
  , "noop"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx -35"
  , "addx 1"
  , "addx 24"
  , "addx -19"
  , "addx 1"
  , "addx 16"
  , "addx -11"
  , "noop"
  , "noop"
  , "addx 21"
  , "addx -15"
  , "noop"
  , "noop"
  , "addx -3"
  , "addx 9"
  , "addx 1"
  , "addx -3"
  , "addx 8"
  , "addx 1"
  , "addx 5"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx -36"
  , "noop"
  , "addx 1"
  , "addx 7"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "addx 6"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx 7"
  , "addx 1"
  , "noop"
  , "addx -13"
  , "addx 13"
  , "addx 7"
  , "noop"
  , "addx 1"
  , "addx -33"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "noop"
  , "noop"
  , "noop"
  , "addx 8"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx 17"
  , "addx -9"
  , "addx 1"
  , "addx 1"
  , "addx -3"
  , "addx 11"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx -13"
  , "addx -19"
  , "addx 1"
  , "addx 3"
  , "addx 26"
  , "addx -30"
  , "addx 12"
  , "addx -1"
  , "addx 3"
  , "addx 1"
  , "noop"
  , "noop"
  , "noop"
  , "addx -9"
  , "addx 18"
  , "addx 1"
  , "addx 2"
  , "noop"
  , "noop"
  , "addx 9"
  , "noop"
  , "noop"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx -37"
  , "addx 1"
  , "addx 3"
  , "noop"
  , "addx 15"
  , "addx -21"
  , "addx 22"
  , "addx -6"
  , "addx 1"
  , "noop"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx -10"
  , "noop"
  , "noop"
  , "addx 20"
  , "addx 1"
  , "addx 2"
  , "addx 2"
  , "addx -6"
  , "addx -11"
  , "noop"
  , "noop"
  , "noop"
  ]
