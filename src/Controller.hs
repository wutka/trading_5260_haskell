module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import Data.List

runGame :: CountryResources -> Int -> Int -> Int -> Double -> [Transform] -> [ScoreParameter] ->
  Int -> [CountryResources] -> [CountryResources]
runGame cr maxDepth frontierSize numSchedules gamma transforms scoring numTurns acc =
  
  where
    countries = Map.keys cr
    plannerConfig country = PlannerConfig country (delete country countries) maxDepth frontierSize numSchedules gamma scoring transforms
    plannerConfigs = map plannerConfig countries

runRound :: CountryResources -> [PlannerConfig] -> CountryResources
runRound = undefined
    
