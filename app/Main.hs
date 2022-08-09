module Main where

import Resources
import Loader
import Planner
import Controller
import System.Environment ( getArgs )
import Data.List
import qualified Data.Map as Map

-- Prints the results for a single country in a single round
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
    -- Extract the country's resource data from the resource data map
    countryRm = cr Map.! country

    -- Convert the resources to a string of name-value pairs
    countryResourcesStr = unwords $ map showRes $ Map.toList countryRm

    -- Represent a resource and value as name:value
    showRes (s,i) = s ++ ":" ++ show i

    -- Represent an updated resource as update type: resource = amount
    showUpdate (UpdatedResource d s i) = d ++ ": " ++ s ++ "=" ++ show i

    -- If there was no valid schedule, just show "No Valid Schedule"
    showSchedule Nothing = ["No valid schedule"]

    -- Otherwise, turn each item in the schedule into a string
    showSchedule (Just schedule) = map show schedule

    -- Represent a scoring detail as the scoringType field val
    scoreDetail (ScoreDetail scoreType field val) = scoreType ++" "++field++" "++show val

    -- Turn the scoring details into a list of strings
    scoreDetails = map scoreDetail details

    -- Retrieves only the scoring element from a PlannerConfig structure
    getScoring (PlannerConfig _ _ _ _ _ _ scoring _ _ _) = scoring

    -- Get the scoring for this country from the PlannerConfig
    scoring = getScoring $ pcMap Map.! country

-- Prints the results of a single round    
printRound :: (Map.Map String PlannerConfig) -> [String] -> (Int,Map.Map String RoundResult) -> IO ()
printRound pcMap countries (round,resultMap) = do
  -- Print the round number
  putStrLn $ "Round: "++show round

  -- Call printRoundResult for each member of roundResults
  mapM_ (printRoundResult pcMap) roundResults
  
  where

    -- Fetch the round results for each country
    roundResults = map lookupCountry countries

    -- Fetch the round results for a specific country
    lookupCountry country = resultMap Map.! country

-- At startup we try to verify all the transforms to make sure they contain
-- valid resource names. This function checks to make sure a resource is
-- contained in the initial resource map
checkResource :: Map.Map String Int -> ResourceAmount -> IO ()
checkResource rm (ResourceAmount res _) = do  
  if Map.notMember res rm then
    putStrLn ("Invalid resource name: "++res)
  else
    return ()

-- For a transform, make sure all the inputs and outputs contain valid fields
-- Note: Since Transfer structs are not read in from external files, we don't
-- need a corresponding check for them
checkTransform :: Map.Map String Int -> Transform -> IO ()
checkTransform rm (Transform country inputs outputs) = do
  mapM_ (checkResource rm) inputs
  mapM_ (checkResource rm) outputs

-- Loads all the files needed for the PlannerConfig for a single country
-- The country data files are expected to be in data/countries with specific names
-- for each file type.
loadPlannerConfig :: CountryResources -> String -> IO PlannerConfig
loadPlannerConfig cr country = do
  -- Load the transforms that the AI can do
  transforms <- loadTransforms (countryDir++"transforms")

  -- Load the transforms that run automatically each turn
  autoTransforms <- loadTransforms (countryDir++"auto_transforms")

  -- Load the scoring rules (each country can have its own scoring)
  scoring <- loadScoringFormula (countryDir++"scoring.txt")

  -- Load the rules for updating resources each turn (any updates
  -- that can affect multiple resources are defined as auto_transforms
  -- instead.
  updates <- loadUpdates (countryDir++"updates.txt")

  -- Load other config settings, currently maxDepth (depthBound), frontierSize,
  -- numSchedules, and gamma
  items <- loadConfigFile (countryDir++"config.txt")

  -- Make sure the transforms all have valid field names
  let rm = cr Map.! country
  mapM_ (checkTransform rm) transforms
  mapM_ (checkTransform rm) autoTransforms

  -- Create a basic PlannerConfig, and then iterate through the items from config.txt, updating
  -- the specified field
  return $ foldl' handleConfigLine (PlannerConfig country otherCountries 4 200 1 1.0 scoring transforms autoTransforms updates) items
  
  where

    -- Country data files are in a specific place
    countryDir = "data/countries/"++country++"/"

    -- Extract the list of countries to use from the resources structure
    countries = Map.keys cr

    -- Remove this country from the list of countries to create a list of the "other" countries
    otherCountries = delete country countries

-- For each item from config.txt, look at the name of the item, and update PlannerConfig
-- with the specified value.
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

  -- Load the country resources
  (countries,nontransfer) <- loadCountryResources resourcesFile
  plannerConfigs <- mapM (loadPlannerConfig countries) (Map.keys countries)
  let plannerConfigMap = Map.fromList (map plannerCountryPair plannerConfigs)  
  
  -- Compute the schedules
  let turnResults = startGame countries nontransfer plannerConfigs turns
  mapM_ (printRound plannerConfigMap (Map.keys countries)) $ zip [1..] turnResults
  -- Print the schedules
--  putStrLn $ "[" ++ unlines (map show schedule) ++ "]"
  where
    plannerCountryPair pc@(PlannerConfig country _ _ _ _ _ _ _ _ _) = (country,pc)
