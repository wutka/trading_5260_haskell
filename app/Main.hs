module Main where

import Resources
import Loader
import Planner
import Controller
import System.Environment ( getArgs )
import Data.List
import qualified Data.Map as Map

printRoundResult :: (Map.Map String PlannerConfig) -> RoundResult -> IO ()
printRoundResult pcMap (RoundResult country score cr details updates autoTransforms schedule) = do
  putStrLn $ country++": "++show score
  putStrLn $ countryResourcesStr
  putStrLn "Score Details"
  putStrLn $ unlines scoreDetails
  putStrLn "Updates:"
  putStrLn $ unlines $ map showUpdate updates
  putStrLn "Auto Transforms:"
  putStrLn $ unlines $ map show autoTransforms
  putStrLn "Schedule:"
  putStrLn $ unlines $ showSchedule schedule
  where
    countryRm = cr Map.! country
    countryResourcesStr = unwords $ map showRes $ Map.toList countryRm
    showRes (s,i) = s ++ ":" ++ show i
    showUpdate (UpdatedResource d s i) = d ++ ": " ++ s ++ "=" ++ show i
    showSchedule Nothing = ["No valid schedule"]
    showSchedule (Just schedule) = map show schedule
    scoreDetail (ScoreDetail scoreType field val) = scoreType ++" "++field++" "++show val
    scoreDetails = map scoreDetail details    
    getScoring (PlannerConfig _ _ _ _ _ _ scoring _ _ _) = scoring
    scoring = getScoring $ pcMap Map.! country
    
printRound :: (Map.Map String PlannerConfig) -> [String] -> (Int,Map.Map String RoundResult) -> IO ()
printRound pcMap countries (round,resultMap) = do
  putStrLn $ "Round: "++show round
  mapM_ (printRoundResult pcMap) roundResults
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

loadPlannerConfig :: CountryResources -> String -> IO PlannerConfig
loadPlannerConfig cr country = do
  transforms <- loadTransforms (countryDir++"transforms")
  autoTransforms <- loadTransforms (countryDir++"auto_transforms")
  scoring <- loadScoringFormula (countryDir++"scoring.txt")
  updates <- loadUpdates (countryDir++"updates.txt")
  items <- loadConfigFile (countryDir++"config.txt")

  let rm = cr Map.! country
  mapM_ (checkTransform rm) transforms
  mapM_ (checkTransform rm) autoTransforms

  return $ foldl' handleConfigLine (PlannerConfig country otherCountries 4 200 1 1.0 scoring transforms autoTransforms updates) items
  where
    countryDir = "data/countries/"++country++"/"
    countries = Map.keys cr
    otherCountries = delete country countries

handleConfigLine :: PlannerConfig -> (String, CsvItem) -> PlannerConfig
handleConfigLine pc ("maxDepth", CsvInt maxDepth) = pc { depthBound=maxDepth }
handleConfigLine pc ("maxDepth", _) = error "Invalid data type for maxDepth, expected int"
handleConfigLine pc ("frontierSize", CsvInt frontierSize) = pc {frontierMaxSize=frontierSize}
handleConfigLine pc ("frontierSize", _) = error "Invalid data type for frontierSize, expected int"
handleConfigLine pc ("numSchedules", CsvInt numSchedules) = pc {numSchedules=numSchedules}
handleConfigLine pc ("numSchedules", _) = error "Invalid data type for numSchedules, expected int"
handleConfigLine pc ("gamma", CsvDouble gamma) = pc {gamma=gamma}
handleConfigLine pc ("gamma", _) = error "Invalid data type for gamma, expected double"
handleConfigLine pc (field,_) = error ("Unknown config field "++field)

main :: IO ()
main = do
  argv <- getArgs
  -- Command-line args:
  -- transforms-directory scoring-file resources-file self-country max-depth frontier-size num-schedules gamma
  let turns = if length argv > 0 then read (head argv) else 10
  let resourcesFile = if length argv > 1 then argv !! 1 else "data/resources_part2.csv"

  let transformsDir = if length argv > 0 then head argv else "data/transforms"
  let autoTransformsDir = if length argv > 1 then argv !! 1 else "data/auto_transforms"
  let updatesFile = if length argv > 2 then argv !! 2 else "data/updates.txt"
  let scoringFile = if length argv > 3 then argv !! 3 else "data/scoring2.txt"

  

  let maxDepth = if length argv > 6 then read (argv !! 6) else 4
  let frontierSize = if length argv > 7 then read (argv !! 7) else 200
  let numSchedules = if length argv > 8 then read (argv !! 8) else 1
  let gamma = if length argv > 9 then read (argv !! 9) else 1.0

  -- Load the country resources
  countries <- loadCountryResources resourcesFile
  plannerConfigs <- mapM (loadPlannerConfig countries) (Map.keys countries)
  let plannerConfigMap = Map.fromList (map plannerCountryPair plannerConfigs)  
  
  -- Compute the schedules
  let turnResults = startGame countries plannerConfigs turns
  mapM_ (printRound plannerConfigMap (Map.keys countries)) $ zip [1..] turnResults
  -- Print the schedules
--  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
  where
    plannerCountryPair pc@(PlannerConfig country _ _ _ _ _ _ _ _ _) = (country,pc)
  
