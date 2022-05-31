module Loader where

import System.Directory
import Data.List
import Resources
import Parser
import Lexer
import qualified Data.Map as Map

loadTransform :: String -> IO SpecItem
loadTransform filename = do
  contents <- readFile filename
  return $ opParser $ lexer contents
  
loadTransforms :: String -> IO [SpecItem]
loadTransforms dir = do
  files <- getDirectoryContents dir
  let templates = map ((dir ++ "/") ++) $ filter (isSuffixOf ".tmpl") files
  mapM loadTransform templates


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
  
