{-# LANGUAGE RankNTypes #-}

module Solution
  ( Solution,
    empty,
    solutionS,
    solutionG,
    adventSolution,
  )
where

data PartialSolution d d1 d2 r1 r2 = PartialSolution
  { readFrom :: FilePath -> IO d,
    solution1 :: d1 -> r1,
    solution2 :: d2 -> r2
  }

newtype Solution d = Solution {solve :: PartialSolution d d d (IO ()) (IO ())}

adventSolution :: FilePath -> Solution a -> IO ()
adventSolution file = run . solve
  where
    run PartialSolution {..} = do
      input <- readFrom ("input/2020/" <> file <> ".txt")
      solution1 input
      solution2 input

empty :: Solution String
empty = Solution emptyP

emptyP :: forall d1 d2 r1 r2. PartialSolution String d1 d2 r1 r2
emptyP =
  PartialSolution
    { readFrom = readFile,
      solution1 = error "Not Implemented",
      solution2 = error "Not Implemented"
    }

-- | Convinient way to create a 'Solution'.
solutionS ::
  (Show b, Show c) => (String -> d) -> (d -> b) -> (d -> c) -> Solution d
solutionS p s1 s2 = Solution . part1 s1 . part2 s2 $ parserS p emptyP

-- | Convinient way to create a 'Solution' with input parsing from file.
solutionG ::
  (Show b, Show c) => (FilePath -> IO d) -> (d -> b) -> (d -> c) -> Solution d
solutionG p s1 s2 = Solution . part1 s1 . part2 s2 $ emptyP {readFrom = p}

-- | Setter for parsing input string.
parserS ::
  (String -> d) ->
  PartialSolution x d1 d2 r1 r2 ->
  PartialSolution d d1 d2 r1 r2
parserS p s = s {readFrom = fmap p . readFile}

-- | Setter for part one.
part1 ::
  Show b =>
  (d -> b) ->
  PartialSolution d x d2 y r2 ->
  PartialSolution d d d2 (IO ()) r2
part1 f s = s {solution1 = print . f}

-- | Setter for part two.
part2 ::
  Show c =>
  (d -> c) ->
  PartialSolution d d1 x r1 y ->
  PartialSolution d d1 d r1 (IO ())
part2 f s = s {solution2 = print . f}