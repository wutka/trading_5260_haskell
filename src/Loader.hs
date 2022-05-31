module Loader where

import System.Directory
import Data.List
import Data.Maybe
import Resources
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


loadCSV :: String -> IO [[CsvItem]]
loadCSV filename = do
  contents <- readFile filename
  return $ map parseLine $ lines contents
  where
    parseLine l = csvLineParser $ lexer l

loadCountryResources :: String -> IO CountryResources
loadCountryResources filename = do
  csvInfo <- loadCSV filename
  let fields = map getCsvString $ tail $ head csvInfo
  return $ Map.fromList $ map (makeResourceList fields) $ tail csvInfo
  where
    makeResourceList fields countryNumbers =
      (getCsvString (head countryNumbers) , Map.fromList (zip fields (map getCsvInt (tail countryNumbers))))

loadScoringFormula :: String -> IO [ScoreParameter]
loadScoringFormula filename = do
  csvInfo <- loadCSV filename
  return $ map parseScoreParameter (tail csvInfo)

parseScoreParameter :: [CsvItem] -> ScoreParameter
parseScoreParameter [fieldItem,weightItem,constantItem,CsvString "Ratio",ratioField] =
  RatioScore (getCsvString fieldItem) (getCsvDouble weightItem) (getCsvDouble constantItem)
             (getCsvString ratioField)
parseScoreParameter x =
  error ("Unable to parse scoring parameter: "++show x)
  
