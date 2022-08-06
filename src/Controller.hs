module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import Data.List
import Debug.Trace

data RoundResult = RoundResult String CountryResources [UpdatedResource] [Operation] (Maybe [ScheduleItem])

startGame :: CountryResources -> Int -> Int -> Int -> Double -> [Transform] -> [Transform] -> [ResourceUpdate] -> [ScoreParameter] ->
  Int -> [Map.Map String RoundResult]
startGame cr maxDepth frontierSize numSchedules gamma transforms autoTransforms resourceUpdates scoring numTurns =
  runGame cr plannerConfigs numTurns []
  where
    countries = Map.keys cr
    plannerConfig country = PlannerConfig country (delete country countries) maxDepth frontierSize numSchedules gamma scoring transforms autoTransforms resourceUpdates
    plannerConfigs = map plannerConfig countries

runGame :: CountryResources -> [PlannerConfig] -> Int -> [Map.Map String RoundResult] -> [Map.Map String RoundResult]
runGame cr pc roundsLeft acc =
  if roundsLeft < 1 then
    reverse acc
  else
    runGame newCountryResources pc (roundsLeft-1) (roundMap:acc)
  where
    (newCountryResources, roundMap) = runRound cr plannerMap pc Map.empty
    plannerMap = Map.fromList $ map makeKVPair pc
    makeKVPair pc@(PlannerConfig country _ _ _ _ _ _ _ _ _) = (country,pc)
    
runRound :: CountryResources -> Map.Map String PlannerConfig -> [PlannerConfig] -> Map.Map String RoundResult -> (CountryResources, Map.Map String RoundResult)
runRound cr pcMap [] roundMap = (cr, roundMap)
runRound cr pcMap (self@(PlannerConfig country _ _ _ _ _ _ _ autoTransforms resourceUpdates):rest) roundMap =
  runRound resultCr pcMap rest updatedRoundMap
  where
    (updatedRm, updates) = applyResourceUpdates (cr Map.! country) resourceUpdates
    updatedCr = Map.insert country updatedRm cr
    autoTransforms = computeAutoTransforms updatedCr self
    autoCr = foldl' applyAutoOp updatedCr autoTransforms
    applyAutoOp cr op = applyOp cr op 1
    acceptedSchedule = chooseSchedule updatedCr pcMap schedules
    schedules = computeSchedule updatedCr self
    resultCr = maybe updatedCr (applySchedule updatedCr) acceptedSchedule
    updatedRoundMap = Map.insert country (RoundResult country resultCr updates autoTransforms acceptedSchedule) roundMap

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
  
computeAutoTransforms :: CountryResources -> PlannerConfig -> [Operation]
computeAutoTransforms cr (PlannerConfig self _ _ _ _ _ _ _ autoTransforms _) =
  map multiplyAutoOp $ filter nonZeroMult bestAutoTransforms
  where
    selfTransforms = map (createTransformOp self 1) autoTransforms
    bestAutoTransforms = map tryAutoTransform selfTransforms
    tryAutoTransform trans = (greatestMultiplier cr trans, trans)
    nonZeroMult (n,t) = n > 0
    multiplyAutoOp (n,op) = multiplyOp op n

