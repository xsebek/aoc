-- |
-- Module      : Day25
-- Description : Solution to AOC 2020 Day 25: Combo Breaker
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/25>
module Day25 where
import Data.List (find)
import Data.Maybe (fromJust)

-- | Solution to Day 25.
main25 :: FilePath -> IO ()
main25 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type Zp = Integer

p :: Zp
p = 20201227

parse :: String -> (Zp, Zp)
parse = take2 . map read . lines
 where
  take2 [x, y] = (x, y)
  take2 _ = error "expecting two numbers"

trans :: Zp -> Zp -> Zp
trans sub loop = sub ^ (loop `mod` (p - 1)) `mod` p

pows2 :: Zp -> [Zp]
pows2 = iterate (`trans` 2)

pows2of7 :: [Zp]
pows2of7 = take (ceiling log2p) $ pows2 7
 where
  log2p :: Double
  log2p = logBase 2 (fromIntegral p)

multFrom2Pows :: Zp -> Zp
multFrom2Pows z = case z `mod` p of
  0 -> 0
  zp -> go pows2of7 zp
 where
  go :: [Zp] -> Zp -> Zp
  go _ 0 = 1
  go [] _ = error $ "log2 P powers of two should be enough for " <> show z
  go (pw2 : pws2) i =
    let r = go pws2 (i `div` 2)
     in if even i then r else pw2 * r `mod` p

type I = Integer

loopSizes :: [(I, Zp)]
loopSizes = zip [0..] $ map multFrom2Pows [0..p-1]

loopSizeIs :: Zp -> I
loopSizeIs pub = fst . fromJust $ find ((==pub) . snd) loopSizes

-- >>> solve1 example
-- 14897079
solve1 :: (Zp, Zp) -> Zp
solve1 (pc, pd) = let (kc, kd) = (loopSizeIs pc, loopSizeIs pd)
  in 7 `trans` (kc * kd)

-- >>> solve2 example
solve2 :: a -> Int
solve2 = undefined

example :: (Zp, Zp)
example = (5764801, 17807724)