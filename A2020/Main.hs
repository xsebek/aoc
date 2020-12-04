module Main where

import Control.Monad (forM_)
import qualified Day01 as D1
import qualified Day02 as D2
import qualified Day03 as D3
import qualified Day04 as D4
import Solution
import System.Environment (getArgs)

main :: IO ()
main =
  do
    arg <- getArgs
    if null arg
      then fail help
      else forM_ arg $ \case
        "-h" -> putStrLn help
        "01" -> adventSolve D1.solution "01"
        "02" -> adventSolve D2.solution "02"
        "03" -> adventSolve D3.solution "03"
        "04" -> adventSolve D4.solution "04"
        c -> fail ("Could not parse day " <> c <> "! " <> help)
  where
    help = "Please select day 01-25."