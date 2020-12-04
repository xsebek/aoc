module Day04 where

import Data.Bool (bool)
import Data.Char
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text, intercalate, splitOn, unpack)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Solution
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

solution :: Solve [[(String, String)]]
solution = solveG (fmap parsing . TIO.readFile) solve1 solve2

parsing :: Text -> [[(String, String)]]
parsing = fmap (block . unpack) . splitOn "\n\n"

block :: String -> [(String, String)]
block = mapMaybe (parseMaybe keyPair) . words

keyPair :: Parser (String, String)
keyPair = do
  key <- some letterChar
  char ':'
  value <- some $ satisfy (not . isSpace)
  return (key, value)


keys :: S.Set String
keys = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" {-, "cid"-}]

solve1 :: [[(String, b)]] -> Int
solve1 = length . filter hasKeys

hasKeys :: [(String, b)] -> Bool
hasKeys l = keys `S.isSubsetOf` S.fromList (map fst l)


solve2 :: [[(String, String)]] -> Int
solve2 = length . filter (all $ uncurry check) . filter hasKeys

inRange :: Int -> Int -> Int -> Bool
inRange l r x = l <= x && x <= r

check :: String -> String -> Bool
check = \case
  "byr" -> inRange 1920 2002 `mayParse` L.decimal
  "iyr" -> inRange 2010 2020 `mayParse` L.decimal
  "eyr" -> inRange 2020 2030 `mayParse` L.decimal
  "hgt" -> validHeight `mayParse` parseHeight
  "hcl" -> checkParse parseHairColor
  "ecl" -> checkParse parseEyeColor
  "pid" -> checkParse (count 9 digitChar)
  "cid" -> const True
  e -> error ("Unknown: " <> e)
  where
    mayParse :: (a -> Bool) -> Parser a -> String -> Bool
    mayParse b p = maybe False b . parseMaybe p
    checkParse :: Parser String -> String -> Bool
    checkParse p = isJust . parseMaybe p
    validHeight = uncurry $ bool (inRange 59 76) (inRange 150 193) . (== "cm")

parseHeight :: Parser (String, Int)
parseHeight = do
  d <- L.decimal
  x <- string "cm" <|> string "in"
  pure (x, d)

parseHairColor :: Parser String
parseHairColor = char '#' >> count 6 lowHex
  where
    lowHex = oneOf $ ['0' .. '9'] <> ['a' .. 'f']

parseEyeColor :: Parser String
parseEyeColor =
  let colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
   in choice $ map string colors

-- >>> solve1 $ parsing example
-- 2
-- >>> map (all $ uncurry check) $ parsing example
-- [True,True,True,True]
example :: Text
example =
  intercalate
    "\n"
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm",
      "",
      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929",
      "",
      "hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm",
      "",
      "hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in"
    ]

-- >>> map (all $ uncurry check) $ parsing exampleInvalid
-- [False,False,False,False]
exampleInvalid :: Text
exampleInvalid =
  intercalate
    "\n"
    [ "eyr:1972 cid:100",
      "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
      "",
      "iyr:2019",
      "hcl:#602927 eyr:1967 hgt:170cm",
      "ecl:grn pid:012533040 byr:1946",
      "",
      "hcl:dab227 iyr:2012",
      "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
      "",
      "hgt:59cm ecl:zzz",
      "eyr:2038 hcl:74454a iyr:2023",
      "pid:3556412378 byr:2007"
    ]

-- >>> map (all $ uncurry check) $ parsing exampleValid
-- [True,True,True,True]
exampleValid :: Text
exampleValid =
  intercalate
    "\n"
    [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
      "hcl:#623a2f",
      "",
      "eyr:2029 ecl:blu cid:129 byr:1989",
      "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
      "",
      "hcl:#888785",
      "hgt:164cm byr:2001 iyr:2015 cid:88",
      "pid:545766238 ecl:hzl",
      "eyr:2022",
      "",
      "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719",
      ""
    ]