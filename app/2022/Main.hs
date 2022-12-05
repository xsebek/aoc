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
-- import Day06 (main06)
-- import Day07 (main07)
-- import Day08 (main08)
-- import Day09 (main09)
-- import Day10 (main10)
-- import Day11 (main11)
-- import Day12 (main12)
-- import Day13 (main13)
-- import Day14 (main14)
-- import Day15 (main15)
-- import Day16 (main16)
-- import Day17 (main17)
-- import Day18 (main18)
-- import Day19 (main19)
-- import Day20 (main20)
-- import Day21 (main21)
-- import Day22 (main22)
-- import Day23 (main23)
-- import Day24 (main24)
-- import Day25 (main25)

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
    , todoDay -- main06
    , todoDay -- main07
    , todoDay -- main08
    , todoDay -- main09
    , todoDay -- main10
    , todoDay -- main11
    , todoDay -- main12
    , todoDay -- main13
    , todoDay -- main14
    , todoDay -- main15
    , todoDay -- main16
    , todoDay -- main17
    , todoDay -- main18
    , todoDay -- main19
    , todoDay -- main20
    , todoDay -- main21
    , todoDay -- main22
    , todoDay -- main23
    , todoDay -- main24
    , todoDay -- main25
    ]
 where
  apply = zipWith (&) paths
  paths = map (\day -> "input/2022/" <> day <> ".txt") days
  todoDay = const $ putStrLn "The selected day is not yet implemented"

-- | Day strings formatted as %02d.
--
-- >>> take 12 days
-- ["01","02","03","04","05","06","07","08","09","10","11","12"]
days :: [String]
days = map (\d -> (if d < (10 :: Int) then ('0' :) else id) (show d)) [1 ..]
