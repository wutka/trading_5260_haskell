module Main where

import Resources
import Loader
import Planner
import System.Environment
import qualified Data.Map as Map

getExcess cr scoring country =
  (country, getExcessResources cr country scoring)

getShortfall cr scoring country =
  (country, getShortfallResources cr country scoring)
  
main :: IO ()
main = do
  argv <- getArgs
  let maxDepth = if length argv > 0 then read (head argv) else 4
  let frontierSize = if length argv > 1 then read (argv !! 1) else 200
  let numSchedules = if length argv > 2 then read (argv !! 2) else 10
  transforms <- loadTransforms "data/transforms"
  scoring <- loadScoringFormula "data/scoring2.txt"
  countries <- loadCountryResources "data/resources.csv"
  let keys = Map.keys countries
  let excesses = map (getExcess countries scoring) keys
  let shortfalls = map (getShortfall countries scoring) keys
  putStrLn $ show scoring
  putStrLn $ show transforms
  putStrLn $ unlines $ ["Excesses:"] ++ map show excesses
  putStrLn $ unlines $ ["ShortFalls:"] ++ map show shortfalls
