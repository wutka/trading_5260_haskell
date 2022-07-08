module Loader where

import System.Directory
import Data.List
import Data.Maybe
import Resources
    ( getTransform,
      getCsvDouble,
      getCsvInt,
      getCsvString,
      CountryResources,
      ScoreParameter(..),
      CsvItem(CsvString),
      Operation,
      Transform )
import Parser
import Lexer
import qualified Data.Map as Map

-- Loads an operation from a file
loadOperation :: String -> IO Operation
loadOperation filename = do
  -- Read the file
  contents <- readFile filename
  -- Parse the operation
  return $ opParser $ lexer contents

-- Loads operations from files ending in .tmpl from the named directory
loadOperations :: String -> IO [Operation]
loadOperations dir = do
  files <- getDirectoryContents dir
  let templates = map ((dir ++ "/") ++) $ filter (isSuffixOf ".tmpl") files
  mapM loadOperation templates

-- Loads all tranforms from files ending in .tmpl from the named directory  
loadTransforms :: String -> IO [Transform]
loadTransforms dir = do
  tlist <- loadOperations dir
  return $ mapMaybe getTransform tlist

-- Loads a CSV file
loadCSV :: String -> IO [[CsvItem]]
loadCSV filename = do
  contents <- readFile filename
  return $ map parseLine $ lines contents
  where
    parseLine l = csvLineParser $ lexer l

-- Parses a CSV file as a map of country resrouces
loadCountryResources :: String -> IO CountryResources
loadCountryResources filename = do
  csvInfo <- loadCSV filename
  -- Get the field names from the first line, removing the first field (country name)
  let fields = map getCsvString $ tail $ head csvInfo
  -- Create a map from the remaining lines
  return $ Map.fromList $ map (makeResourceList fields) $ tail csvInfo
  where
    -- Create a resource map from a CSV line
    makeResourceList fields countryNumbers =
      (getCsvString (head countryNumbers) , Map.fromList (zip fields (map getCsvInt (tail countryNumbers))))

-- Loads a scoring formula from a CSV file
loadScoringFormula :: String -> IO [ScoreParameter]
loadScoringFormula filename = do
  csvInfo <- loadCSV filename
  return $ map parseScoreParameter (tail csvInfo)

parseScoreParameter :: [CsvItem] -> ScoreParameter
-- If the third parameter is "Ratio", this is a ratio score
parseScoreParameter [fieldItem,weightItem,CsvString "Ratio",ratioField] =
  RatioScore (getCsvString fieldItem) (getCsvDouble weightItem) (getCsvString ratioField)
-- If the fourth parameter is TargetedRatio, this is a targeted ratio score
parseScoreParameter [fieldItem,baseScore,weightItem,targetItem,CsvString "TargetedRatio",ratioField] =
  TargetedRatioScore (getCsvString fieldItem) (getCsvDouble baseScore) (getCsvDouble weightItem)
                     (getCsvDouble targetItem) (getCsvString ratioField)
parseScoreParameter x =
  error ("Unable to parse scoring parameter: "++show x)
  
