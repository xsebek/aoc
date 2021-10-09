module Parser (
    Parser,
    parseFile,
    parseExample,
    word,
    P.endBy,
    P.hidden,
    P.option,
    P.sepBy,
    P.sepBy1,
    P.some,
    (P.<?>),
    PC.eol,
    PC.letterChar,
    PC.space,
    PC.string,
    PC.char,
) where

-- Data names
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)

import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC

-- | Default type for nontrivial parsers
type Parser = P.Parsec Void Text

-- | Read and parse file with error reporting in IO
parseFile :: Parser a -> FilePath -> IO a
parseFile p f = parsedEitherToIO . P.runParser p f =<< T.readFile f

parseExample :: Parser a -> Text -> IO a
parseExample p t = parsedEitherToIO $ P.runParser p "Example" t

parsedEitherToIO :: Either (P.ParseErrorBundle Text Void) a -> IO a
parsedEitherToIO = either (fail . P.errorBundlePretty) pure

word :: Parser String
word = P.some PC.letterChar P.<?> "word"
