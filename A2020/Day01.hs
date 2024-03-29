module Day01 where

main01 :: FilePath -> IO ()
main01 f = do
  input <- map read . lines <$> readFile f
  print $ solve1 input
  print $ solve2 input

ix :: [a] -> [(Int, a)]
ix = zip [0 ..]

-- >>> solve1 [1, 2019, 2020]
-- 2019
solve1 :: [Int] -> Int
solve1 xs =
  head
    [ x * y
      | (i, x) <- ix xs,
        (j, y) <- ix xs,
        i /= j,
        x + y == 2020
    ]

-- >>> solve2 [0, 1, 2019, 2020]
-- 0
solve2 :: [Int] -> Int
solve2 xs =
  head
    [ x * y * z
      | (i, x) <- ix xs,
        (j, y) <- ix xs,
        i /= j,
        (k, z) <- ix xs,
        i /= k,
        j /= k,
        x + y + z == 2020
    ]