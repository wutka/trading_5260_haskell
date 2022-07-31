module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import Data.List
import Debug.Trace

startGame :: CountryResources -> Int -> Int -> Int -> Double -> [Transform] -> [ResourceUpdate] -> [ScoreParameter] ->
  Int -> [CountryResources]
startGame cr maxDepth frontierSize numSchedules gamma transforms resourceUpdates scoring numTurns =
  runGame cr plannerConfigs numTurns []
  where
    countries = Map.keys cr
    plannerConfig country = PlannerConfig country (delete country countries) maxDepth frontierSize numSchedules gamma scoring transforms resourceUpdates
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
    makeKVPair pc@(PlannerConfig country _ _ _ _ _ _ _ _) = (country,pc)
    
runRound :: CountryResources -> Map.Map String PlannerConfig -> [PlannerConfig] -> CountryResources
runRound cr pcMap [] = cr
runRound cr pcMap (self@(PlannerConfig country _ _ _ _ _ _ _ resourceUpdates):rest) =
  trace "Running Round" runRound (maybe updatedCr (applySchedule updatedCr) acceptedSchedule) pcMap rest
  where    
    updatedCr = trace "Updating cr" Map.insert country (applyResourceUpdates (cr Map.! country) resourceUpdates) cr
    acceptedSchedule = trace ("Choosing schedule") chooseSchedule updatedCr pcMap schedules
    schedules = trace "Computing schedules" computeSchedule updatedCr self

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


  
  
    
