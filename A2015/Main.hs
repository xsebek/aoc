module Main where

import Data.Maybe (listToMaybe)
import qualified Day01 as D1
import qualified Day02 as D2
import qualified Day03 as D3
import qualified Day04 as D4
import qualified Day05 as D5
import qualified Day06 as D6
import Solution
import System.Environment (getArgs)

main :: IO ()
main =
  do
    arg <- getArgs
    let a = listToMaybe arg
    case a of
      Just "01" -> adventSolve D1.solution "01"
      Just "02" -> adventSolve D2.solution "02"
      Just "03" -> adventSolve D3.solution "03"
      Just "04" -> adventSolve D4.solution "04"
      Just "05" -> adventSolve D5.solution "05"
      Just "06" -> adventSolve D6.solution "06"
      _ -> fail "Please select day (01-25)."

adventSolve :: Solution a b -> String -> IO ()
adventSolve s f = do
  input <- parse s <$> s `readFrom` ("input/2015/" <> f <> ".txt")
  s `solution1` input
  s `solution2` input