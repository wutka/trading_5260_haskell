module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import Data.List

startGame :: CountryResources -> Int -> Int -> Int -> Double -> [Transform] -> [ScoreParameter] ->
  Int -> [CountryResources]
startGame cr maxDepth frontierSize numSchedules gamma transforms scoring numTurns =
  runGame cr plannerConfigs numTurns []
  where
    countries = Map.keys cr
    plannerConfig country = PlannerConfig country (delete country countries) maxDepth frontierSize numSchedules gamma scoring transforms
    plannerConfigs = map plannerConfig countries

runGame :: CountryResources -> [PlannerConfig] -> Int -> [CountryResources] -> [CountryResources]
runGame cr pc roundsLeft acc =
  if roundsLeft < 1 then
    reverse acc
  else
    runGame newCountryResources pc (roundsLeft-1) (newCountryResources:acc)
  where
    newCountryResources = runRound cr plannerMap pc
    plannerMap = Map.fromList $ map makeKVPair pc
    makeKVPair pc@(PlannerConfig country _ _ _ _ _ _ _) = (country,pc)
    
runRound :: CountryResources -> Map.Map String PlannerConfig -> [PlannerConfig] -> CountryResources
runRound cr pcMap [] = cr
runRound cr pcMap (self:rest) =
  maybe cr (applySchedule cr) acceptedSchedule
  where
    acceptedSchedule = chooseSchedule cr pcMap schedules
    schedules = computeSchedule cr self

chooseSchedule :: CountryResources -> Map.Map String PlannerConfig -> [[ScheduleItem]] -> Maybe [ScheduleItem]
chooseSchedule cr pcMap [] = Nothing
chooseSchedule cr pcMap (sched:schedRest) =
  if all (acceptsSchedule sched) (Map.elems pcMap) then
    Just sched
  else
    chooseSchedule cr pcMap schedRest
  where
    acceptsSchedule sched pc =
      acceptSchedule cr pc sched


  
  
    
