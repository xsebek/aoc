module Main where

import Advent2020
import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

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
    failMessage day = fail $ "Could not parse day " <> day <> "! " <> help
    findMain day = snd <$> find ((== day) . fst) ds

solutionsM :: [IO ()]
solutionsM =
  apply
    [ main01,
      main02,
      main03,
      main04,
      main05,
      main06,
      main07,
      main08,
      main09,
      main10,
      main11,
      main12,
      main13
    ]
  where
    apply = zipWith (&) paths
    paths = map (\day -> "input/2020/" <> day <> ".txt") days

-- | Day strings formatted as %02d.
--
-- >>> take 12 days
-- ["01","02","03","04","05","06","07","08","09","10","11","12"]
days :: [String]
days = map (\d -> (if d < 10 then ('0' :) else id) (show d)) [1 ..]
