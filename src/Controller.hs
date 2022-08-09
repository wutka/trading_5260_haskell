module Controller where

import Planner
import Resources
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Debug.Trace

-- The Controller module runs the game/simulation for the country resources and contry configurations
-- provided. It records the results of each country's round in a RoundResult structure and returns
-- a list of maps, where each map contains country-roundresult pairs.

-- Define the data to be saved for each round
data RoundResult = RoundResult String Double CountryResources [ScoreDetail] [UpdatedResource] [Operation] (Maybe [ScheduleItem])

-- Start running the game with the country resources, set of non-transferable resources,
-- planner configurations (one-per-country) and the number of rounds, with -1 indicating
-- that it should run until no countries produce a valid schedule.
startGame :: CountryResources -> Set.Set String -> [PlannerConfig] -> Int -> [Map.Map String RoundResult]
startGame cr nontransfer plannerConfigs numTurns =
  runGame cr nontransfer plannerConfigs numTurns []
  where
    countries = Map.keys cr

-- Run the game for a specified number of rounds, or until no schedules are produced
runGame :: CountryResources -> Set.Set String -> [PlannerConfig] -> Int -> [Map.Map String RoundResult] -> [Map.Map String RoundResult]
runGame cr nontransfer pc roundsLeft acc =
  -- Since the rounds are accumulated onto the front of the list, reverse the
  -- list before returning it
  if roundsLeft == 0 then
    reverse acc

  -- If roundsLeft is negative, keep going until no schedules, and if there
  -- are no schedules, reverse the list before returning it
  else if roundsLeft < 0 then
    if noSchedules then
      reverse acc
    else
      runGame newCountryResources nontransfer pc roundsLeft (roundMap:acc)
  else

    -- Otherwise, recurse to the next round in the game (all the computations
    -- for this round are computed in the where clauses below)
    runGame newCountryResources nontransfer pc (roundsLeft-1) (roundMap:acc)
    
  where
    
    -- Run the round for all countries
    (newCountryResources, roundMap) = runRound cr nontransfer plannerMap pc Map.empty

    -- Create a map from country to planner configs
    plannerMap = Map.fromList $ map makeKVPair pc

    -- Create a pair of country and planner config (used to make a map)
    makeKVPair pc@(PlannerConfig country _ _ _ _ _ _ _ _ _) = (country,pc)

    -- There are no schedules if all the round results have Nothing for the schedule
    noSchedules = all noSchedule $ Map.elems roundMap
    noSchedule (RoundResult _ _ _ _ _ _ Nothing) = True
    noSchedule (RoundResult _ _ _ _ _ _ (Just _)) = False

-- Run a round for the next country in the list of countryes    
runRound :: CountryResources -> Set.Set String -> Map.Map String PlannerConfig -> [PlannerConfig] -> Map.Map String RoundResult -> (CountryResources, Map.Map String RoundResult)

-- If the list of countries is empty, return the results for the round
runRound cr nontransfer pcMap [] roundMap = (cr, roundMap)

runRound cr nontransfer pcMap (self@(PlannerConfig country _ _ _ _ _ scoring _ autoTransforms resourceUpdates):rest) roundMap =
--  trace ("\nOriginal RM for "++country++": "++show (cr Map.! country)++"\nAfter Updates "++country++": "++show (updatedCr Map.! country)++"\nAfter auto "++country++": "++show (autoCr Map.! country)++"\n")
  -- Tail call to the next round, the actual round is computed in the where clause below
  runRound resultCr nontransfer pcMap rest updatedRoundMap
  
  where

    -- Perform the resource updates first (first doesn't mean the order here in
    -- the where clauses, but that it starts with the country resource map contained
    -- in the cr variable passed to the function
    (updatedRm, updates) = applyResourceUpdates (cr Map.! country) resourceUpdates

    -- Create a new country resource map containing the updated resources
    -- for this country
    updatedCr = Map.insert country updatedRm cr

    -- Next, perform any applicable automatic transforms
    (autoCr,autoTransformed) = computeAutoTransforms updatedCr self

    -- Then, for all the possible schedules, compute the first one acceptable
    -- to all parties involved
    acceptedSchedule = chooseSchedule autoCr pcMap schedules

    -- Get a list of possible schedules
    schedules = computeSchedule autoCr nontransfer self

    -- If the accepted schedule is not Nothing, apply it to the autoCr
    -- country resource map, otherwise, just return autoCr as the
    -- new map
    resultCr = maybe autoCr (applySchedule autoCr) acceptedSchedule

    -- Compute the score for this country
    (score,details) = computeScore (resultCr Map.! country) scoring

    -- Insert this country's results into the round result map
    updatedRoundMap = Map.insert country (RoundResult country score resultCr details updates (reverse autoTransformed) acceptedSchedule) roundMap

-- Choose the first schedule accepted by all countries
chooseSchedule :: CountryResources -> Map.Map String PlannerConfig -> [[ScheduleItem]] -> Maybe [ScheduleItem]

-- If there are no schedules left to try, return Nothing
chooseSchedule cr pcMap [] = Nothing
chooseSchedule cr pcMap (sched:schedRest) =

-- If everyone accepts this schedule, return it
  if all (acceptsSchedule sched) (Map.elems pcMap) then
    Just sched

-- Otherwise, make a tail call to try the next schedule in the list
  else
    chooseSchedule cr pcMap schedRest
  where
    acceptsSchedule sched pc =
      acceptSchedule cr pc sched

-- Try applying all the auto transforms for this country
computeAutoTransforms :: CountryResources -> PlannerConfig -> (CountryResources,[Operation])
computeAutoTransforms cr (PlannerConfig self _ _ _ _ _ _ _ autoTransforms _) =
  -- Try each auto transform
  foldl' applyAutoOp (cr,[]) autoOps
  
  where

    -- Make each transform refer specifically to this country
    autoOps = map (createTransformOp self 1) autoTransforms

-- See if an auto transform can be applied to a country and if so, what is
-- the best multiplier for it
applyAutoOp :: (CountryResources,[Operation]) -> Operation -> (CountryResources,[Operation])
applyAutoOp (cr,ops) op =

  -- If the best multiplier is non-zero, add it to the list of auto transforms to perform
  if bestMultiplier > 0 then
    (applyOp cr multipliedOp 1, multipliedOp:ops)
  else
    -- Otherwise, don't change the list of auto transforms to perform
    (cr, ops)

  where
    -- Apply the bestMultiplier to the op
    multipliedOp = multiplyOp op bestMultiplier

    -- Find the multiplier that allows the maximum application of the transform
    -- The idea being that if there are 20 renewable energy plants, you want
    -- all 20 to generate power if they can, and if there aren't enough resources
    -- to run all 20, run as many as possible
    bestMultiplier = greatestMultiplier cr op
