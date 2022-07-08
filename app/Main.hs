module Main where

import Resources
import Loader
import Planner
import System.Environment

main :: IO ()
main = do
  argv <- getArgs
  let maxDepth = if length argv > 0 then read (head argv) else 4
  let frontierSize = if length argv > 1 then read (argv !! 1) else 200
  let numSchedules = if length argv > 2 then read (argv !! 2) else 10
  transforms <- loadTransforms "data/transforms"
  scoring <- loadScoringFormula "data/scoring2.txt"
  countries <- loadCountryResources "data/resources.csv"
  let schedule = computeSchedule countries "Atlantis" maxDepth frontierSize numSchedules transforms scoring
  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
