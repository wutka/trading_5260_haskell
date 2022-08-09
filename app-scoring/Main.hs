module Main where

import Resources
import Loader
import Planner
import System.Environment
import qualified Data.Map as Map

transformScore countries country scoring op =
  (op, score)
  where
    newCr = applyOp countries op 1
    rm = newCr Map.! country
    (score,_) = computeScore rm scoring
    
main :: IO ()
main = do
  argv <- getArgs
  let maxDepth = if length argv > 0 then read (head argv) else 4
  let frontierSize = if length argv > 1 then read (argv !! 1) else 200
  let numSchedules = if length argv > 2 then read (argv !! 2) else 10
  transforms <- loadTransforms "data/transforms"
  scoring <- loadScoringFormula "data/scoring2.txt"
  (countries,_) <- loadCountryResources "data/resources.csv"
  let rm = countries Map.! "Atlantis"
  let (startScore,_) = computeScore rm scoring
  let ops = map (createTransformOp "Atlantis" 1) transforms
  putStrLn $ "Original score: "++show startScore
  putStrLn $ unlines (map (show . transformScore countries "Atlantis" scoring) ops)
  
  

