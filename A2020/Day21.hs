-- |
-- Module      : Day21
-- Description : Solution to AOC 2020 Day 21: Allergen Assessment
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/21>
module Day21 where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Void (Void)
import Data.Text (Text)
import Data.List ( intercalate )
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import Data.Maybe (mapMaybe, fromMaybe)

-- | Solution to Day 21.
main21 :: FilePath -> IO ()
main21 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

type Ingredient = String
type Allergen = String
type IngredientList = ([Ingredient], [Allergen])

type Parser = P.Parsec Void Text

parseFile :: Parser a -> FilePath -> IO a
parseFile p f = eToIO . P.runParser p f =<< T.readFile f
  where
    eToIO = either (fail . P.errorBundlePretty) pure

-- >>> map (\(i,a)-> (length i, length a)) <$> P.parseMaybe parser example
-- Just [(4,2),(4,1),(2,1),(3,1)]
parser :: Parser [IngredientList]
parser = parseIngredientList `P.endBy` PC.eol

pWord :: Parser String
pWord = P.some PC.alphaNumChar

-- >>> P.parseMaybe ingredients "x y z "
-- Just ["x","y","z"]
ingredients :: Parser [Ingredient]
ingredients = P.some (pWord <* PC.space)

-- >>> P.parseMaybe allergens "a, b"
-- Just ["a","b"]
allergens :: Parser [Allergen]
allergens = pWord `P.sepBy1` (PC.char ',' <* PC.space)

-- >>> P.parseMaybe parAle "(contains x)"
-- Just ["x"]
parAle :: Parser [Allergen]
parAle = PC.string "(contains" *> PC.space *> allergens <* PC.char ')'

-- >>> P.parseMaybe parseIngredientList "x y z (contains a, b)"
-- Just (["x","y","z"],["a","b"])
parseIngredientList :: Parser IngredientList
parseIngredientList = (,) <$> ingredients <*> parAle

-- >>> solve1 <$> P.parseMaybe parser example
-- Just 5
solve1 :: [IngredientList] -> Int
solve1 iss = length noAle
  where
    ass = reduceAllergenSets iss
    noAle = removePotentialAllergens (Set.unions $ Map.elems ass) iss

type AllergenSets = Map Allergen (Set Ingredient)

-- potential ingredients with allergens (starting with all)
toAllergenSet :: IngredientList -> AllergenSets
toAllergenSet (is, as) = let iss = Set.fromList is in Map.fromList (map (,iss) as)

-- reduce to minimal set of ingredients with set intersect
-- >>> reduceAllergenSets <$> P.parseMaybe parser example
-- Just (fromList [("dairy",fromList ["mxmxvkd"]),("fish",fromList ["mxmxvkd","sqjhc"]),("soy",fromList ["fvjkl","sqjhc"])])
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

-- >>> solve2 <$> P.parseMaybe parser example
-- Just "mxmxvkd,sqjhc,fvjkl"
solve2 :: [IngredientList] -> String
solve2 = intercalate "," . map snd . head . validAssign . reduceAllergenSets

type Assigned = [(Allergen, Ingredient)]

-- >>>  validAssign . reduceAllergenSets <$> P.parseMaybe parser example
-- Just [[("dairy","mxmxvkd"),("fish","sqjhc"),("soy","fvjkl")]]
validAssign :: AllergenSets -> [Assigned]
validAssign ass = fromMaybe [] $ go Set.empty $ map (fmap Set.toList) $ Map.assocs ass
  where
    -- choose one in is and filter
    go :: Set Ingredient -> [(Allergen, [Ingredient])] -> Maybe [Assigned]
    go used = \case
      [] -> Just [[]]
      (a, is):xs -> let m = concat $ mapMaybe (chooseAndFilter used xs a) is
                    in if null m && not (null xs) then Just [] else Just m
    chooseAndFilter :: Set Ingredient
      -> [(Allergen, [Ingredient])]
      -> Allergen
      -> Ingredient
      -> Maybe [Assigned]
    chooseAndFilter used xs a i =
      if i `Set.member` used
        then Nothing
        else map ((a,i):) <$> go (Set.insert i used) xs

example :: Text
example =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)\n"
