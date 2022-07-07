module Main where

import Resources
import Loader
import Planner


main :: IO ()
main = do
  transforms <- loadTransforms "data/transforms"
  scoring <- loadScoringFormula "data/scoring2.txt"
  countries <- loadCountryResources "data/resources.csv"
  let schedule = computeSchedule countries "Atlantis" 6 1000 10 transforms scoring
  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
