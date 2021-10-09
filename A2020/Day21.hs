-- |
-- Module      : Day21
-- Description : Solution to AOC 2020 Day 21: Allergen Assessment
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/21>
module Day21 where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Parser

-- Data names
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

-- Data
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

-- | Solution to Day 21.
main21 :: FilePath -> IO ()
main21 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

type Ingredient = String
type Allergen = String
type IngredientList = ([Ingredient], [Allergen])

-- >>> map (\(i,a)-> (length i, length a)) <$> parseExample parser example
-- [(4,2),(4,1),(2,1),(3,1)]
parser :: Parser [IngredientList]
parser = parseIngredientList `endBy` eol

-- >>> parseExample ingredients "x y z "
-- ["x","y","z"]
ingredients :: Parser [Ingredient]
ingredients = some ((word <?> "ingredient name") <* space)

-- >>> parseExample allergens "a, b"
-- ["a","b"]
allergens :: Parser [Allergen]
allergens = (word <?> "allergen name") `sepBy1` (char ',' <* space)

-- >>> parseExample parAle "(contains x)"
-- ["x"]
parAle :: Parser [Allergen]
parAle = string "(contains" *> hidden space *> allergens <* char ')'

-- >>> parseExample parseIngredientList "x y z (contains a, b)"
-- (["x","y","z"],["a","b"])
parseIngredientList :: Parser IngredientList
parseIngredientList = (,) <$> ingredients <*> option [] parAle

-- >>> solve1 <$> parseExample parser example
-- 5
solve1 :: [IngredientList] -> Int
solve1 iss = length noAle
 where
  ass = reduceAllergenSets iss
  noAle = removePotentialAllergens (Set.unions $ Map.elems ass) iss

type AllergenSets = Map Allergen (Set Ingredient)

-- | potential ingredients with allergens (starting with all)
toAllergenSet :: IngredientList -> AllergenSets
toAllergenSet (is, as) = let iss = Set.fromList is in Map.fromList (map (,iss) as)

-- | Reduce to minimal set of ingredients with set intersect.
--
-- >>> reduceAllergenSets <$> parseExample parser example
-- fromList [("dairy",fromList ["mxmxvkd"]),("fish",fromList ["mxmxvkd","sqjhc"]),("soy",fromList ["fvjkl","sqjhc"])]
reduceAllergenSets :: [IngredientList] -> AllergenSets
reduceAllergenSets = foldr (reduceAle . toAllergenSet) Map.empty
 where
  reduceAle :: AllergenSets -> AllergenSets -> AllergenSets
  reduceAle = Map.foldrWithKey (\a set -> Map.alter (Just . newInter set) a)
  newInter :: Set Ingredient -> Maybe (Set Ingredient) -> Set Ingredient
  newInter set = maybe set (Set.intersection set)

-- | Filter with (pottential allergen inside) ingredient set membership
removePotentialAllergens :: Set Ingredient -> [IngredientList] -> [Ingredient]
removePotentialAllergens pot = filter (not . (`Set.member` pot)) . concatMap fst

-- >>> solve2 <$> parseExample parser example
-- "mxmxvkd,sqjhc,fvjkl"
solve2 :: [IngredientList] -> String
solve2 = intercalate "," . map snd . head . validAssign . reduceAllergenSets

type Assigned = [(Allergen, Ingredient)]

-- >>>  validAssign . reduceAllergenSets <$> parseExample parser example
-- [[("dairy","mxmxvkd"),("fish","sqjhc"),("soy","fvjkl")]]
validAssign :: AllergenSets -> [Assigned]
validAssign ass = fromMaybe [] $ go Set.empty $ map (fmap Set.toList) $ Map.assocs ass
 where
  -- choose one in is and filter
  go :: Set Ingredient -> [(Allergen, [Ingredient])] -> Maybe [Assigned]
  go used = \case
    [] -> Just [[]]
    (a, is) : xs ->
      let m = concat $ mapMaybe (chooseAndFilter used xs a) is
       in if null m && not (null xs) then Just [] else Just m
  chooseAndFilter ::
    Set Ingredient ->
    [(Allergen, [Ingredient])] ->
    Allergen ->
    Ingredient ->
    Maybe [Assigned]
  chooseAndFilter used xs a i =
    if i `Set.member` used
      then Nothing
      else map ((a, i) :) <$> go (Set.insert i used) xs

example :: Text
example =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)\n"
