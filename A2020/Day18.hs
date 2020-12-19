-- |
-- Module      : Day18
-- Description : Solution to AOC 2020 Day 18: Operation Order
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2020/day/18>
module Day18 where

import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L

data Op = M | A deriving (Eq, Show)

data Expr = I Int | E Expr Op Expr deriving (Eq, Show)

-- | Solution to Day 18.
main18 :: FilePath -> IO ()
main18 f = do
  input <- lines <$> readFile f
  print =<< solve1 input
  print =<< solve2 input

--------------------------------------------------------------------
--                            PARSE                               --
--------------------------------------------------------------------

type Parser = Parsec Void String

-- >>> ex3
-- "2 * 3 + (4 * 5)"
-- >>> prettyE <$> parse parseExpr1 ex3
-- "(2 * 3) + (4 * 5)"
parse :: MonadFail m => Parser a -> String -> m a
parse p = parseOrFail (p <* eof)

parseOp :: Parser Op
parseOp = (M <$ char '*') <|> (A <$ char '+') <?> "operator +/*"

parseI :: Parser Expr
parseI = I <$> decimal <?> "integer"

--------------------------------------------------------------------
--                            PART1                               --
--------------------------------------------------------------------

-- >>> solve1 $ lines example
-- 26457
solve1 :: [String] -> IO Int
solve1 l = sum . map eval <$> mapM (parse parseExpr1) l

-- >>> prettyE <$> parse parseExpr1 "3 + (4 * 5)"
-- "3 + (4 * 5)"
parseExpr1 :: Parser Expr
parseExpr1 = parseE >>= continue
  where
    continue :: Expr -> Parser Expr
    continue left = continue' left <|> pure left
    continue' :: Expr -> Parser Expr
    continue' left = do
      op <- sp parseOp
      right <- sp parseE
      continue (E left op right)
    parseE :: Parser Expr
    parseE = parseI <|> parens parseExpr1 <?> "integer or (expression)"

-- >>> map eval <$> mapM (parse parseExpr1) (lines example)
-- [71,51,26,437,12240,13632]
eval :: Expr -> Int
eval = \case
  I i -> i
  E l o r ->
    let op = case o of A -> (+); M -> (*)
     in eval l `op` eval r

--------------------------------------------------------------------
--                            PART2                               --
--------------------------------------------------------------------

-- >>> solve2 $ lines example
-- 694173
solve2 :: [String] -> IO Int
solve2 l = sum . map eval <$> mapM (parse parseExpr2) l

-- | Same as parsing in part one except hungrily consume all additions.
--
-- >>> prettyE <$> parse parseExpr2 "1 + 2 * 3 + 4"
-- "(1 + 2) * (3 + 4)"
parseExpr2 :: Parser Expr
parseExpr2 = parseAE >>= continue
  where
    continue :: Expr -> Parser Expr
    continue left = (<|> pure left) $ do
      M <- sp parseOp
      right <- parseAE
      continue (E left M right)
    -- take while plus
    parseAE :: Parser Expr
    parseAE = sp parseE >>= parseAdd
    parseAdd :: Expr -> Parser Expr
    parseAdd left = (<|> pure left) . try $ do
      A <- sp parseOp
      right <- sp parseE
      parseAdd (E left A right)
    parseE :: Parser Expr
    parseE = parseI <|> parens parseExpr2 <?> "integer or (expression)"

--------------------------------------------------------------------
--                            DEBUG                               --
--------------------------------------------------------------------

example :: String
example = unlines [ex1, ex2, ex3, ex4, ex5, ex6]

ex1 :: String
ex1 = "1 + 2 * 3 + 4 * 5 + 6"

ex2 :: String
ex2 = "1 + (2 * 3) + (4 * (5 + 6))"

ex3 :: String
ex3 = "2 * 3 + (4 * 5)"

ex4 :: String
ex4 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"

ex5 :: String
ex5 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

ex6 :: String
ex6 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

parseOrFail :: MonadFail m => Parser a -> String -> m a
parseOrFail p = eitherPretty . runParser p "Day18"

eitherPretty :: MonadFail m => Either (ParseErrorBundle String Void) a -> m a
eitherPretty = either (fail . errorBundlePretty) pure

sp :: Parser a -> Parser a
sp p = space *> p <* space

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

prettyE :: Expr -> String
prettyE = \case
  I i -> show i
  E l o r -> unwords [showE l, showO o, showE r]
  where
    showE (I a) = show a
    showE e = '(' : prettyE e <> ")"
    showO A = "+"
    showO M = "*"
