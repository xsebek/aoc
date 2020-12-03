module Main where

import Data.Maybe (listToMaybe)
import qualified Day01 as D1
import qualified Day02 as D2
import qualified Day03 as D3
import qualified Day04 as D4
import qualified Day05 as D5
import qualified Day06 as D6
import System.Environment (getArgs)

main :: IO ()
main =
  do
    arg <- getArgs
    let a = listToMaybe arg
    case a of
      Just "01" -> D1.main
      Just "02" -> D2.main
      Just "03" -> D3.main
      Just "04" -> D4.main
      Just "05" -> D5.main
      Just "06" -> D6.main
      _ -> fail "Please select day (01-25)."
