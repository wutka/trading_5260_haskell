module Main where

import Resources
import Loader
import Planner
import Controller
import System.Environment
import qualified Data.Map as Map

printRoundResult :: RoundResult -> IO ()
printRoundResult (RoundResult country cr updates autoTransforms schedule) = do
  putStrLn country
  putStrLn $ countryResourcesStr
  putStrLn "Updates:"
  putStrLn $ unlines $ map showUpdate updates
  putStrLn "Auto Transforms:"
  putStrLn $ unlines $ map show autoTransforms
  putStrLn "Schedule:"
  putStrLn $ unlines $ showSchedule schedule
  where
    countryResourcesStr = unwords $ map showRes $ Map.toList (cr Map.! country)
    showRes (s,i) = s ++ ":" ++ show i
    showUpdate (UpdatedResource s i) = s ++ ":" ++ show i
    showSchedule Nothing = ["No valid schedule"]
    showSchedule (Just schedule) = map show schedule
    
  
printRound :: [String] -> (Int,Map.Map String RoundResult) -> IO ()
printRound countries (round,resultMap) = do
  putStrLn $ "Round: "++show round
  mapM_ printRoundResult roundResults
  where
    roundResults = map lookupCountry countries
    lookupCountry country = resultMap Map.! country

checkResource :: Map.Map String Int -> ResourceAmount -> IO ()
checkResource rm (ResourceAmount res _) = do  
  if Map.notMember res rm then
    putStrLn ("Invalid resource name: "++res)
  else
    return ()

checkTransform :: Map.Map String Int -> Transform -> IO ()
checkTransform rm (Transform country inputs outputs) = do
  mapM_ (checkResource rm) inputs
  mapM_ (checkResource rm) outputs


main :: IO ()
main = do
  argv <- getArgs
  -- Command-line args:
  -- transforms-directory scoring-file resources-file self-country max-depth frontier-size num-schedules gamma
  let transformsDir = if length argv > 0 then head argv else "data/transforms"
  let autoTransformsDir = if length argv > 1 then argv !! 1 else "data/auto_transforms"
  let updatesFile = if length argv > 2 then argv !! 2 else "data/updates.txt"
  let scoringFile = if length argv > 3 then argv !! 3 else "data/scoring2.txt"
  let resourcesFile = if length argv > 4 then argv !! 4 else "data/resources_part2.csv"
  let turns = if length argv > 5 then read (argv !! 5) else 10
  let maxDepth = if length argv > 6 then read (argv !! 6) else 5
  let frontierSize = if length argv > 7 then read (argv !! 7) else 200
  let numSchedules = if length argv > 8 then read (argv !! 8) else 1
  let gamma = if length argv > 9 then read (argv !! 9) else 1.0

  -- Load the transforms from the directory
  transforms <- loadTransforms transformsDir
  -- Load the auto transforms from the directory
  autoTransforms <- loadTransforms autoTransformsDir
  -- Load the per-turn updates
  updates <- loadUpdates updatesFile
  -- Load the scoring data
  scoring <- loadScoringFormula scoringFile
  -- Load the country resources
  countries <- loadCountryResources resourcesFile

  let rm = countries Map.! (head $ Map.keys countries)
  mapM_ (checkTransform rm) transforms
  mapM_ (checkTransform rm) autoTransforms
  
  -- Compute the schedules
  let turnResults = startGame countries maxDepth frontierSize numSchedules gamma transforms autoTransforms updates scoring turns
  mapM_ (printRound (Map.keys countries)) $ zip [1..] turnResults
  -- Print the schedules
--  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
  
