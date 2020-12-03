module Solution (Solution (..), emptySolution, setPart1, setPart2) where

data Solution i d = Solution
  { readFrom :: FilePath -> IO i,
    parse :: i -> d,
    solution1 :: d -> IO (),
    solution2 :: d -> IO ()
  }

emptySolution :: Solution String String
emptySolution =
  Solution
    { readFrom = readFile,
      parse = id,
      solution1 = const $ fail "Not Implemented",
      solution2 = const $ fail "Not Implemented"
    }

setPart1 :: Show b => (d -> b) -> Solution i d -> Solution i d
setPart1 f s = s {solution1 = print . f}

setPart2 :: Show b => (d -> b) -> Solution i d -> Solution i d
setPart2 f s = s {solution2 = print . f}