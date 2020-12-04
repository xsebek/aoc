{-# LANGUAGE RankNTypes #-}

module Solution where

import Control.Monad ((>=>))

data Solution d d1 d2 r1 r2 = Solution
  { readFrom :: FilePath -> IO d,
    solution1 :: d1 -> r1,
    solution2 :: d2 -> r2
  }

type Solve d = Solution d d d (IO ()) (IO ())

adventSolve :: Solve a -> String -> IO ()
adventSolve s f = do
  input <- s `readFrom` ("input/2020/" <> f <> ".txt")
  s `solution1` input
  s `solution2` input

empty :: forall d1 d2 r1 r2. Solution String d1 d2 r1 r2
empty =
  Solution
    { readFrom = readFile,
      solution1 = error "Not Implemented",
      solution2 = error "Not Implemented"
    }

-- | Convinient way to create a 'Solution'.
solve :: (Show b, Show c) => (String -> d) -> (d -> b) -> (d -> c) -> Solve d
solve p s1 s2 = setPart1 s1 . setPart2 s2 $ setParserS p empty

-- | Setter for parsing input from file.
solveG ::
  (Show b, Show c) => (FilePath -> IO d) -> (d -> b) -> (d -> c) -> Solve d
solveG p s1 s2 = setPart1 s1 . setPart2 s2 $ empty {readFrom = p}

-- | Setter for parsing input string.
setParserS :: (String -> d) -> Solution x d1 d2 r1 r2 -> Solution d d1 d2 r1 r2
setParserS p s = s {readFrom = readFile >=> pure . p}

-- | Setter for part one.
setPart1 ::
  Show b => (d -> b) -> Solution d x d2 y r2 -> Solution d d d2 (IO ()) r2
setPart1 f s = s {solution1 = print . f}

-- | Setter for part two.
setPart2 ::
  Show c => (d -> c) -> Solution d d1 x r1 y -> Solution d d1 d r1 (IO ())
setPart2 f s = s {solution2 = print . f}