module Main where

import Control.Monad (forM_)
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
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
        s
          | s `elem` ds -> let x = fromJust (sToFun s) in snd x (fst x)
          | otherwise -> fail ("Could not parse day " <> s <> "! " <> help)
  where
    ds = days 6
    help = "Please select day 01-25."
    sToFun s = find ((s ==) . fst) $ zip ds solutions

solutions =
  let as = adventSolve
   in [ as D01.solution,
        as D02.solution,
        as D03.solution,
        as D04.solution,
        as D05.solution,
        as D06.solution
      ]

days :: Int -> [String]
days m = map (\d -> bool id ('0' :) (d < 10) $ show d) [1 .. m]
