module Day07 where

import Control.Monad (liftM2)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Map (Map, traverseMaybeWithKey, (!))
import qualified Data.Map as Map
import Data.Monoid (Sum (Sum), getSum)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Simple parser of strings.
type Parser = Parsec Void String

-- | The type of bag color, for example of "shiny gold".
type Color = String

-- | The set of colors.
type Colors = Set.Set Color

-- | Bags must colored and contain specific number of other bags.
--
-- Due to recent aviation regulations, many rules (your puzzle input) are being
-- enforced about bags and their contents; bags must be color-coded and must
-- contain specific quantities of other color-coded bags.
-- Apparently, nobody responsible for these regulations considered how long
-- they would take to enforce!
data Bag = Bag {color :: Color, contain :: [(Int, Color)]} deriving (Eq, Show)

main07 :: FilePath -> IO ()
main07 f = do
  input <- parse f =<< readFile f
  print $ solve1 input
  print $ solve2 input

-------------------------------------------------------------------------

-- | Parse bag rules.
--
-- For example, consider the following rules:
--
-- > light red bags contain 1 bright white bag, 2 muted yellow bags.
-- > dark orange bags contain 3 bright white bags, 4 muted yellow bags.
-- > bright white bags contain 1 shiny gold bag.
-- > muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
-- > shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
-- > dark olive bags contain 3 faded blue bags, 4 dotted black bags.
-- > vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
-- > faded blue bags contain no other bags.
-- > dotted black bags contain no other bags.
--
-- These rules specify the required contents for 9 bag types. In this
-- example, every @faded blue@ bag is empty, every @vibrant plum@ bag
-- contains 11 bags (5 @faded blue@ and 6 @dotted black@), and so on.
--
-- >>> const "OK" <$> parse "example" example
-- "OK"
parse :: FilePath -> String -> IO [Bag]
parse f = mapM (eToFail . runParser parseBag f) . lines

-- >>> eToFail (Right 1) :: IO Int
-- 1
eToFail :: MonadFail m => Either (ParseErrorBundle String Void) a -> m a
eToFail = either (fail . errorBundlePretty) pure

parseBag :: Parser Bag
parseBag = do
  color <- parseBagColor
  _ <- string " contain "
  contain <- ([] <$ string "no other bags") <|> (nColor `sepBy` string ", ")
  _ <- char '.' <* space
  pure $ Bag {..}
  where
    nColor = (,) <$> L.decimal <* space1 <*> parseBagColor

parseBagColor :: Parser Color
parseBagColor = do
  l <- some letterChar
  _ <- space1
  r <- some letterChar
  _ <- space1
  string "bag"
  _ <- optional $ char 's'
  pure (l ++ ' ' : r)

-------------------------------------------------------------------------

-- | Part One.
--
-- You have a @shiny gold@ bag. If you wanted to carry it in at least one
-- other bag, how many different bag colors would be valid for the
-- outermost bag? (In other words: how many colors can, eventually, contain
-- at least one @shiny gold@ bag?)
--
-- In the above rules, the following options would be available to you:
--
-- - A @bright white@ bag, which can hold your @shiny gold@ bag directly.
-- - A @muted yellow@ bag, which can hold your @shiny gold@ bag directly,
--    plus some other bags.
-- - A @dark orange@ bag, which can hold @bright white@ and
--    @muted yellow@ bags, either of which could then hold your
--    @shiny gold@ bag.
-- - A @light red@ bag, which can hold @bright white@ and @muted yellow@
--    bags, either of which could then hold your @shiny gold@ bag.
--
-- In this example, the number of bag colors that can eventually
-- contain at least one @shiny gold@ bag is @4@.
--
-- /How many bag colors can contain at least one @shiny gold@ bag?/
--
-- >>> solve1 <$> parse "ex" example
-- 4
solve1 :: [Bag] -> Int
solve1 = Set.size . containColor gold . bagsMap

-- | Color of my bag.
gold :: Color
gold = "shiny gold"

type BagMap = Map Color Colors

-- | Find all colors that map to color.
--
-- 0. (drop all empty)
-- 1. find all with c in set (and remove them from map)
-- 2. for each left Bag
--  - if bag contains color in set, add it to result set (and remove from map)
--  - else keep
-- 3. repeat 2. and 3. with new set
-- 4. union of result sets
--
-- >>> containColor gold . bagsMap <$> parse "ex" example
-- fromList ["bright white","dark orange","light red","muted yellow"]
containColor :: Color -> BagMap -> Colors
containColor col m = go (Set.singleton col) (filterEmpty m)
  where
    -- 1.
    filterEmpty :: BagMap -> BagMap
    filterEmpty = runIdentity . traverseMaybeWithKey (\_ -> Identity . noNull)
    noNull :: Colors -> Maybe Colors
    noNull s = if null s then Nothing else Just s
    -- 2.
    takeInter :: Colors -> Color -> Colors -> (Colors, Maybe Colors)
    takeInter c k s =
      if c `Set.disjoint` s
        then (Set.empty, Just s) -- keep
        else (Set.singleton k, Nothing) -- put in result and remove
        -- 3. and 4.
    go :: Colors -> BagMap -> Colors
    go s m =
      let (ns, nm) = traverseMaybeWithKey (takeInter s) m
       in if null ns then ns else ns `Set.union` go ns nm

bagsMap :: [Bag] -> BagMap
bagsMap = Map.fromList . map (liftM2 (,) color containedColors)
  where
    containedColors = Set.fromList . map snd . contain

-------------------------------------------------------------------------

-- | Part Two.
--
-- It\'s getting pretty expensive to fly these days - not because of ticket
-- prices, but because of the ridiculous number of bags you need to buy!
--
-- Consider again your @shiny gold@ bag and the rules in part one.
--
-- A single @shiny gold@ bag must contain 1 @dark olive@ bag (and the 7
-- bags within it) plus 2 @vibrant plum@ bags (and the 11 bags within
-- /each/ of those): @1 + 1*7 + 2 + 2*11@ = @32@ bags!
--
-- Of course, the actual rules have a small chance of going several levels
-- deeper than this example; be sure to count all of the bags, even if the
-- nesting becomes topologically impractical!
--
-- Here\'s another example:
--
-- > shiny gold bags contain 2 dark red bags.
-- > dark red bags contain 2 dark orange bags.
-- > dark orange bags contain 2 dark yellow bags.
-- > dark yellow bags contain 2 dark green bags.
-- > dark green bags contain 2 dark blue bags.
-- > dark blue bags contain 2 dark violet bags.
-- > dark violet bags contain no other bags.
--
-- In this example, a single @shiny gold@ bag must contain @126@ other bags.
--
-- /How many individual bags are required inside your single @shiny gold@ bag?/
--
-- >>> solve2 <$> parse "ex" example
-- 32
-- >>> solve2 <$> parse "ex2" example2
-- 126
solve2 :: [Bag] -> Int
solve2 = bagCount gold . bagsCountMap

type BagIntMap = Map Color [(Int, Color)]

bagCount :: Color -> BagIntMap -> Int
bagCount c m = getSum . foldMap toSum $ m ! c
  where
    toSum :: (Int, Color) -> Sum Int
    toSum (i, nc) = Sum $ i * (1 + bagCount nc m)

bagsCountMap :: [Bag] -> BagIntMap
bagsCountMap = Map.fromList . map (liftM2 (,) color contain)

-------------------------------------------------------------------------

example :: String
example =
  unlines
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      "bright white bags contain 1 shiny gold bag.",
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      "faded blue bags contain no other bags.",
      "dotted black bags contain no other bags."
    ]

example2 :: String
example2 =
  unlines
    [ "shiny gold bags contain 2 dark red bags.",
      "dark red bags contain 2 dark orange bags.",
      "dark orange bags contain 2 dark yellow bags.",
      "dark yellow bags contain 2 dark green bags.",
      "dark green bags contain 2 dark blue bags.",
      "dark blue bags contain 2 dark violet bags.",
      "dark violet bags contain no other bags."
    ]