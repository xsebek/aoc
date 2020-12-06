module Day02 where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import Solution
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

solution :: Solution [Password]
solution = solutionG (parseFile passwords) solve1 solve2

type Parser = Parsec Void Text

parseFile :: Parser a -> FilePath -> IO a
parseFile p f = eToIO . runParser p f =<< T.readFile f
  where
    eToIO = either (fail . errorBundlePretty) pure

data Policy = P {low :: Int, high :: Int, c :: Char} deriving (Eq, Show)

data Password = Pwd {policy :: Policy, password :: String} deriving (Eq, Show)

passwords :: Parser [Password]
passwords = parseWord `endBy` eol

parseWord :: Parser Password
parseWord = Pwd <$> parseRange <* char ':' <* space <*> some alphaNumChar

-- >>> parseMaybe parseRange "3-5 c"
-- Just (P {low = 3, high = 5, c = 'c'})
parseRange :: Parser Policy
parseRange = P <$> L.decimal <* char '-' <*> L.decimal <* space <*> letterChar

solve1 :: [Password] -> Int
solve1 = length . filter compliant

-- >>> compliant $ Pwd (P 1 3 'a') "abcde"
-- True
-- >>> compliant $ Pwd (P 1 3 'b') "cdefg"
-- False
-- >>> compliant $ Pwd (P 2 9 'c') "ccccccccc"
-- True
compliant :: Password -> Bool
compliant Pwd {..} = low policy <= cs && cs <= high policy
  where
    cs = length $ filter (== c policy) password

solve2 :: [Password] -> Int
solve2 = length . filter compliant2

-- >>> compliant2 $ Pwd (P 1 3 'a') "abcde"
-- True
-- >>> compliant2 $ Pwd (P 1 3 'b') "cdefg"
-- False
-- >>> compliant2 $ Pwd (P 2 9 'c') "ccccccccc"
-- False
compliant2 :: Password -> Bool
compliant2 Pwd {..} =
  let comply i = password !? (i - 1) == Just (c policy)
   in comply (low policy) /= comply (high policy)

(!?) :: [a] -> Int -> Maybe a
xs !? i = listToMaybe $ drop i xs