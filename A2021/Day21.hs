-- |
-- Module      : Day21
-- Description : Solution to AOC 2021 Day 21: Dirac Dice
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/21>
module Day21 where

import Data.Bifunctor (first, bimap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

-- | Solution to Day 21.
main21 :: FilePath -> IO ()
main21 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> (Int, Int)
parse = getTwo . map getPos . lines
 where
  getPos = read . last . words
  getTwo [a, b] = (a, b)
  getTwo _ = error "There should be two lines!"

type Die = [Int]

fakeDie :: Die
fakeDie = cycle [1 .. 100]

type Pos = Int

data Player = P
  { pos :: Pos
  , score :: Int
  }
  deriving (Eq, Ord, Show, Read)

playerPlus :: Int -> Player -> Player
playerPlus i (P p s) = P np (s + np)
 where
  np = succ $ (i + p - 1) `mod` 10

play :: (Die, Player) -> (Die, Player)
play (d, p) = (nd, playerPlus i p)
 where
  (i, nd) = first sum $ splitAt 3 d

playUntil :: (Player -> Bool) -> (Int, Die, Player, Player) -> (Int, Player, Player)
playUntil won (rolled, d, p1, p2)
  | won np1 = (rolled + 3, np1, p2)
  | won np2 = (rolled + 6, np2, np1)
  | otherwise = playUntil won (6 + rolled, nd2, np1, np2)
 where
  (nd1, np1) = play (d, p1)
  (nd2, np2) = play (nd1, p2)

gscore :: (Int, Player, Player) -> Int
gscore (r, _, p) = r * score p

-- >>> solve1 example
-- 739785
solve1 :: (Int, Int) -> Int
solve1 (i, j) = gscore $ playUntil ((>= 1000) . score) (0, fakeDie, P i 0, P j 0)

type Positions = ((Player, Count), [(Player, Count)])

data Won
  = -- | Partially evaluated score with a list of unknown positions
    Eval (Int, Int) [Positions]
  | -- | Known score
    Score (Int, Int)
  deriving (Eq, Show, Read)

getScore :: Won -> (Int,Int)
getScore w = case w of
  Score s -> s
  Eval s [] -> s
  Eval _ _ -> error $ "Unevaluated score: " <> show w

-- | Knowledge of wins in position.
--
-- > Map : (Pos, Pos) * 3^(3+3) --> (Count, Count) ~ eval [(Pos,Pos)]
type AllWins = Map (Player, Player) Won

-- | Count scores from position, sharing knowledge of wins.
--
-- > Play : (Pos,Pos) --> (Count, Count) + eval [(Pos,Pos)]
playAll :: (Player -> Bool) -> (Player, Player) -> AllWins -> AllWins
playAll won pos allWins = case evalScore' of
  Score _ -> allWins'
  Eval s ps -> evalNext s allWins' (expandPos ps)
 where
  -- | New (partially evaluated) score.
  evalScore :: Won
  evalScore = evalPositions won $ getAll3Pos pos
  -- | Score preferring already known score.
  evalScore' :: Won
  evalScore' = fromMaybe evalScore (Map.lookup pos allWins)
  -- | Adjusted knowledge with (new) score of current position.
  allWins' :: AllWins
  allWins' = Map.insert pos evalScore' allWins
  -- | Expand positions for evalNext (see its comment).
  expandPos :: [Positions] -> [((Player,Count), (Player,Count))]
  expandPos = let f (p, ps) = (p,) <$> ps in concatMap f
  -- | Force evaluation of list of positions, passing on knowledge.
  --
  -- The idea is that all these positions are not instantly winning,
  -- so it is safe to multiply results of each by count of this position.
  evalNext :: (Int, Int) -> AllWins -> [((Player,Count), (Player,Count))] -> AllWins
  evalNext s@(s1, s2) aws = \case
    [] -> Map.insert pos (Score s) aws
    ((p1, c1), (p2, c2)) : ps ->
      let nextAws = playAll won p aws
          (ps1, ps2) = getScore $ nextAws Map.! p
          p = (p1,p2)
       in evalNext (s1 + c1 * c2 * ps1, s2 + c1 * c2 * ps2) nextAws ps

evalPositions :: (Player -> Bool) -> [Positions] -> Won
evalPositions won = uncurry Eval . first sumWins . partitionEithers . concatMap getWin
 where
  getWin :: Positions -> [Either (Int,Int) Positions]
  getWin ((p1, c1),p2s) = if won p1
    then [Left (c1,0)]
    else list2 . bimap (Left . sumWins) (Right . ((p1,c1),)) . partitionEithers $ map (getP2Wins c1) p2s
  getP2Wins :: Count -> (Player, Count) -> Either (Int,Int) (Player, Count)
  getP2Wins c1 (p2,c2) = if won p2 then Left (0, c1*c2) else Right (p2,c2) 
  sumWins :: [(Int, Int)] -> (Int,Int)
  sumWins = bimap sum sum . unzip
  list2 (a,b) = [a,b]

getAll3Pos :: (Player, Player) -> [Positions]
getAll3Pos (p1, p2) = map (,allPs p2) $ allPs p1
 where
  allPs p = first (`playerPlus` p) <$> all3s

all3s :: [(Int, Count)]
all3s = count $ sum3 <$> d3 <*> d3 <*> d3
  where
  d3 = [1..3]
  sum3 a b c = a + b + c

-- >>> solve2 example
-- 444356092776315
solve2 :: (Int,Int) -> Int
solve2 (i, j) = wins $ playAll ((>=21) . score) pos mempty
 where
  wins = uncurry max . getScore . (Map.! pos)
  pos = (P i 0, P j 0)

example :: (Int, Int)
example =
  parse . unlines $
    [ " Player 1 starting position: 4"
    , " Player 2 starting position: 8"
    ]

type Count = Int

count :: Ord a => [a] -> [(a, Int)]
count = Map.toList . go mempty
 where
  go m = \case 
   [] -> m
   x : xs -> go (Map.insertWith (+) x 1 m) xs