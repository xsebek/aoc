{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Day01 (main01)
import Day02 (main02)
import Day03 (main03)
import Day04 (main04)
import Day05 (main05)
import Day06 (main06)
import Day07 (main07)
import Day08 (main08)
import Day09 (main09)
import Day10 (main10)
import Day11 (main11)
import Day12 (main12)
import Day13 (main13)
import Day14 (main14)
import Day15 (main15)
import Day16 (main16)
import Day17 (main17)
import Day18 (main18)
import Day19 (main19)
import Day20 (main20)
import Day21 (main21)
import Day22 (main22)
import Day23 (main23)
import Day24 (main24)
import Day25 (main25)

main :: IO ()
main =
  do
    arg <- getArgs
    if null arg
      then fail help
      else forM_ arg $ \case
        "-h" -> putStrLn help
        day -> failMessage day `fromMaybe` findMain day
 where
  ds = zip days solutionsM
  help = "Please select day 01-25."
  failMessage day = fail $ "Could not parse day '" <> day <> "'! " <> help
  findMain day = snd <$> find ((== day) . fst) ds

solutionsM :: [IO ()]
solutionsM =
  apply
    [ main01
    , main02
    , main03
    , main04
    , main05
    , main06
    , main07
    , main08
    , main09
    , main10
    , main11
    , main12
    , main13
    , main14
    , main15
    , main16
    , main17
    , main18
    , main19
    , main20
    , main21
    , main22
    , main23
    , main24
    , main25
    ]
 where
  apply = zipWith (&) paths
  paths = map (\day -> "input/2021/" <> day <> ".txt") days
  --todoDay = const $ putStrLn "The selected day is not yet implemented"

-- | Day strings formatted as %02d.
--
-- >>> take 12 days
-- ["01","02","03","04","05","06","07","08","09","10","11","12"]
days :: [String]
days = map (\d -> (if d < (10 :: Int) then ('0' :) else id) (show d)) [1 ..]
