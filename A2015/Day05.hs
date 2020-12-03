-- | Solution to AOC 2015 Day 5: Doesn't He Have Intern-Elves For This?
--
-- https://adventofcode.com/2015/day/5
--
-- Santa needs help figuring out which strings in his text file are naughty
-- or nice.
--
-- A nice string is one with all three of the following properties:
--
-- 1. It contains at least three vowels (aeiou only), like aei or xazegov.
-- 2. It contains at least one letter that appears twice in a row, like xx,
--    abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
-- 3. It does not contain the strings ab, cd, pq, or xy, even if they are part
--    of one of the other requirements.
module Day05 where

-- | Process input text file.
main :: IO ()
main = do
  input <- lines <$> readFile "input05.txt"
  print (solve1 input)
  print (solve2 input)

-- Part One.
--
-- >>> solve1 ["ugknbfddgicrmopn", "aaa"]
-- 2
-- >>> solve1 ["jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"]
-- 0
solve1 :: [String] -> Int
solve1 = length . filter allp
  where
    allp line = all ($ line) [atLeast3Vowels, appearsTwice, noBlacklisted]

-- | Property 1.
--
-- >>> atLeast3Vowels "xazegov"
-- True
-- >>> atLeast3Vowels "dvszwmarrgswjxmb" -- only a
-- False
atLeast3Vowels :: String -> Bool
atLeast3Vowels = not . null . drop 2 . filter (`elem` "aeiou")

-- | Property 2.
--
-- >>> appearsTwice "abcdde" -- dd
-- True
appearsTwice :: String -> Bool
appearsTwice s = any (uncurry (==)) $ zip s (tail s)

-- | Property 3.
--
-- >>> noBlacklisted "aabbccdd" -- ab, cd, (pq, xy)
-- False
noBlacklisted :: String -> Bool
noBlacklisted s = not $ any (`elem` ["ab", "cd", "pq", "xy"]) pairs
  where
    pairs = zipWith (\x y -> [x, y]) s (tail s)

-- | Part Two.
--
-- >>> solve2 ["qjhvhtzxzqqjkmpb", "xxyxx"]
-- 2
-- >>> solve2 ["uurcxstgmygtbstg", "ieodomkazucvgmuy"]
-- 0
solve2 :: [String] -> Int
solve2 = length . filter (\s -> twoPairs s && repeatsBetween s)

-- | New Property 1.
--
-- >>> map twoPairs ["xyxy", "aabcdefgaa", "aaa"]
-- [True,True,False]
twoPairs :: String -> Bool
twoPairs = go []
  where
    go pairs (x : y : xs) = [x, y] `elem` drop 1 pairs || go ([x, y] : pairs) (y : xs)
    go _ _ = False

-- | New Property 2.
--
-- >>> repeatsBetween "abcdefeghi"
-- True
repeatsBetween :: String -> Bool
repeatsBetween s = or $ zipWith (==) s (drop 2 s)