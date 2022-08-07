module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Debug.Trace

data RoundResult = RoundResult String Double CountryResources [ScoreDetail] [UpdatedResource] [Operation] (Maybe [ScheduleItem])

startGame :: CountryResources -> Set.Set String -> [PlannerConfig] -> Int -> [Map.Map String RoundResult]
startGame cr nontransfer plannerConfigs numTurns =
  runGame cr nontransfer plannerConfigs numTurns []
  where
    countries = Map.keys cr

runGame :: CountryResources -> Set.Set String -> [PlannerConfig] -> Int -> [Map.Map String RoundResult] -> [Map.Map String RoundResult]
runGame cr nontransfer pc roundsLeft acc =
  if roundsLeft == 0 then
    reverse acc
  else if roundsLeft < 0 then
    if noSchedules then
      reverse acc
    else
      runGame newCountryResources nontransfer pc roundsLeft (roundMap:acc)
  else
    runGame newCountryResources nontransfer pc (roundsLeft-1) (roundMap:acc)
  where
    (newCountryResources, roundMap) = runRound cr nontransfer plannerMap pc Map.empty
    plannerMap = Map.fromList $ map makeKVPair pc
    makeKVPair pc@(PlannerConfig country _ _ _ _ _ _ _ _ _) = (country,pc)
    noSchedules = all noSchedule $ Map.elems roundMap
    noSchedule (RoundResult _ _ _ _ _ _ Nothing) = True
    noSchedule (RoundResult _ _ _ _ _ _ (Just _)) = False
    
runRound :: CountryResources -> Set.Set String -> Map.Map String PlannerConfig -> [PlannerConfig] -> Map.Map String RoundResult -> (CountryResources, Map.Map String RoundResult)
runRound cr nontransfer pcMap [] roundMap = (cr, roundMap)
runRound cr nontransfer pcMap (self@(PlannerConfig country _ _ _ _ _ scoring _ autoTransforms resourceUpdates):rest) roundMap =
--  trace ("\nOriginal RM for "++country++": "++show (cr Map.! country)++"\nAfter Updates "++country++": "++show (updatedCr Map.! country)++"\nAfter auto "++country++": "++show (autoCr Map.! country)++"\n")
  runRound resultCr nontransfer pcMap rest updatedRoundMap
  where
    (updatedRm, updates) = applyResourceUpdates (cr Map.! country) resourceUpdates
    updatedCr = Map.insert country updatedRm cr
    (autoCr,autoTransformed) = computeAutoTransforms updatedCr self
    acceptedSchedule = chooseSchedule autoCr pcMap schedules
    schedules = computeSchedule autoCr nontransfer self
    resultCr = maybe autoCr (applySchedule autoCr) acceptedSchedule
    (score,details) = computeScore (resultCr Map.! country) scoring
    updatedRoundMap = Map.insert country (RoundResult country score resultCr details updates (reverse autoTransformed) acceptedSchedule) roundMap

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
  
computeAutoTransforms :: CountryResources -> PlannerConfig -> (CountryResources,[Operation])
computeAutoTransforms cr (PlannerConfig self _ _ _ _ _ _ _ autoTransforms _) =
  foldl' applyAutoOp (cr,[]) autoOps
  where
    autoOps = map (createTransformOp self 1) autoTransforms

applyAutoOp :: (CountryResources,[Operation]) -> Operation -> (CountryResources,[Operation])
applyAutoOp (cr,ops) op =
  if bestMultiplier > 0 then
    (applyOp cr multipliedOp 1, multipliedOp:ops)
  else
    (cr, ops)
  where
    multipliedOp = multiplyOp op bestMultiplier
    bestMultiplier = greatestMultiplier cr op
