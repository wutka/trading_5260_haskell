module Main where

import Resources
import Loader
import Planner
import System.Environment

main :: IO ()
main = do
  argv <- getArgs
  -- Command-line args:
  -- transforms-directory scoring-file resources-file self-country max-depth frontier-size num-schedules
  let transformsDir = if length argv > 0 then head argv else "data/transforms"
  let scoringFile = if length argv > 1 then argv !! 1 else "data/scoring2.txt"
  let resourcesFile = if length argv > 2 then argv !! 2 else "data/resources.csv"
  let self = if length argv > 3 then argv !! 3 else "Atlantis"
  let maxDepth = if length argv > 4 then read (argv !! 4) else 4
  let frontierSize = if length argv > 5 then read (argv !! 5) else 200
  let numSchedules = if length argv > 6 then read (argv !! 6) else 10
  -- Load the transforms from the directory
  transforms <- loadTransforms transformsDir
  -- Load the scoring data
  scoring <- loadScoringFormula scoringFile
  -- Load the country resources
  countries <- loadCountryResources resourcesFile
  -- Compute the schedules
  let schedule = computeSchedule countries self maxDepth frontierSize numSchedules transforms scoring
  -- Print the schedules
  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
