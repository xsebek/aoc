-- |
-- Module      : Day02
-- Description : Solution to AOC 2021 Day 02: Dive!
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/02>
module Day02 where
import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List (inits)

-- | Solution to Day 02.
main02 :: FilePath -> IO ()
main02 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

data Dir a = Forward a | Up a | Down a
  deriving (Eq, Show, Read)

type D = Dir Int

parse :: String -> [D]
parse = map (read . capital) . lines
  where
    capital (c:cs) = toUpper c : cs
    capital [] = []

-- --------------------------------------

data Pos = P
  { horizontal :: Int
  , depth :: Int
  }
  deriving (Eq, Show)

instance Semigroup Pos where
  (P h1 d1) <> (P h2 d2) = P (h1 + h2) (d1 + d2)

instance Monoid Pos where
  mempty = P 0 0

dirToPos :: D -> Pos
dirToPos d = case d of
  Forward n -> P n 0
  Up n -> P 0 (-n)
  Down n -> P 0 n

pResult :: Pos -> Int
pResult (P h d) = h * d

-- >>> solve1 example
-- 150
solve1 :: [D] -> Int
solve1 = pResult . mconcat . map dirToPos

-- | Aiming position is ssentially @Endo (I,P)@,
--   but we want to run the directions with 'foldl''
--   immediatelly left to right.
type AimPos = (Int, Pos) -> (Int, Pos)

dirToAimPos :: D -> AimPos
dirToAimPos d (a, p) = case d of
  Forward n -> (a, p <> P n (n * a))
  Up n -> (a - n, p)
  Down n -> (a + n, p)

foldAim :: [D] -> (Int, Pos)
foldAim = foldl' (&) (0, mempty) . map dirToAimPos

-- >>> prettyAim example
-- Forward 5  -> aim: 0   P {horizontal = 5, depth = 0}
-- Down 5     -> aim: 5   P {horizontal = 5, depth = 0}
-- Forward 8  -> aim: 5   P {horizontal = 13, depth = 40}
-- Up 3       -> aim: 2   P {horizontal = 13, depth = 40}
-- Down 8     -> aim: 10  P {horizontal = 13, depth = 40}
-- Forward 2  -> aim: 10  P {horizontal = 15, depth = 60}
prettyAim :: [D] -> IO ()
prettyAim ds = do
  putStrLn $ pad "Starting" 10 <> prettyS (0, mempty :: Pos)
  mapM_ pretty . zip ds . drop 1 . map foldAim $ inits ds
 where
   pretty (d, ap) = putStrLn $ padS d 10 <> prettyS ap
   prettyS (a, p) = " -> aim: " <> padS a 5 <> show p
   pad s n = s <> replicate (n - length s) ' '
   padS x n = pad (show x) n 

-- >>> solve2 example
-- 900
solve2 :: [D] -> Int
solve2 = pResult . snd . foldAim

example :: [D]
example = parse . unlines $
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]
