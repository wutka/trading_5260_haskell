module Planner where

import Resources
import Queue
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Map as Map

data PlannerConfig = PlannerConfig {
  country :: String,
  otherCountries :: [String],
  depthBound :: Int,
  frontierMaxSize :: Int,
  numSchedules :: Int,
  gamma :: Double,
  scoring :: [ScoreParameter],
  transforms :: [Transform] }
                   deriving (Eq, Show)

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

-- Compute a list of schedules for the given country with the
-- specified bounding parameters, transforms, and scoring parameters
computeSchedule :: CountryResources -> PlannerConfig -> [[ScheduleItem]]
computeSchedule cm pc@(PlannerConfig self otherCountries depthBound frontierMaxSize numSchedules gamma scoring transforms)  =
  bestSchedules
  where
    baseScore = computeScore (cm Map.! self) scoring
    -- Compute all the schedules
    allSchedules = iterateSchedule cm pc baseScore initQueue scheduleQueue 0
    -- Convert each PlanItem to a schedule
    bestSchedules = map getPISchedule $ allQueueItems allSchedules
    getPISchedule (PlanItem _ _ _ _ s _) = s
    -- Create the frontier
    queue = createQueue frontierMaxSize
    -- Create a bounded priority queue to hold the resulting schedules
    scheduleQueue = createQueue numSchedules
    -- Get the initial set of possible moves
    moves = getMoves pc baseScore (PlanItem 0 0.0 1.0 1.0 [] cm)
    -- Add the initial set of moves to the work queue (if we start with an empty
    -- work queue there would be nothing to do)
    initQueue = foldl' addItem queue moves

-- Gets the next plan state from the queue, and if it hasn't
-- exceeded the depth bounds, finds any moves to make from it
-- While this function is recursive, the recursive calls are
-- all from the tail position so it is equivalent to iteration
iterateSchedule :: CountryResources -> PlannerConfig -> Double -> PriorityQueue -> PriorityQueue -> Int -> PriorityQueue
iterateSchedule cm pc@(PlannerConfig self otherCountries depthBound frontierMaxSize numSchedules gamma scoring transforms) baseScore queue itemQueue iterations =
--  trace ("Next item is "++show nextItem)
  (
-- If the work queue is empty, we are done, show the number of iterations
  if isNothing nextItem then
    trace ("Total iterations: "++show iterations) itemQueue
-- If the current item hits the depth bound, don't compute additional moves
-- just use the currQueue (the one from which this item was removed)
  else if nextDepth >= depthBound then
    iterateSchedule cm pc baseScore currQueue (addItem itemQueue (fromJust nextItem)) (iterations+1)
-- Otherwise compute the next moves (by referencing nextQueue instead of currQueue
-- in the recursive call)  
  else
    iterateSchedule cm pc baseScore nextQueue (addItem itemQueue (fromJust nextItem)) (iterations+1)
  )
  where
    -- Get the next item and updated queue
    (nextItem, currQueue) = getNext queue
    -- Extract the item from the Maybe structure
    (PlanItem nextDepth nextPriority nextGamma nextP nextSchedule _) = fromJust nextItem
    -- Compute the possible next moves from the current item
    moves = getMoves pc baseScore (fromJust nextItem)
    -- Add all the newly generated moves to the frontier (queue)
    nextQueue = foldl' addItem currQueue moves

-- Computes the possible moves from a given position    
getMoves :: PlannerConfig -> Double -> PlanItem -> [PlanItem]
getMoves pc@(PlannerConfig self otherCountries depthBound frontierMaxSize numSchedules gamma scoring transforms) baseScore  (PlanItem currDepth _ currGamma currP currSched planCr) =
  -- For each possible schedule item, create a PlanItem containing
  -- the next depth, the score, the schedule, and a snapshot of the resources
  map (makePlanItem planCr pc baseScore currDepth currGamma currP currSched) allOperations
  where
    -- Create a set of transform operations for the self country
    selfTransformOps = map (createTransformOp self 1) transforms
    -- Find the 10 best multipliers for each transform, concatenate
    -- all the operations into a single list
    transformOperations = concatMap (\x -> take 10 $ bestOperationQuantities planCr self scoring x) selfTransformOps
    -- Find all the transfers from the self country to another
    transferFromSelfOperations = mapMaybe (\c -> getTransfers planCr self self c scoring) otherCountries
    -- Find all the transfers to the self country from another
    transferToSelfOperations = mapMaybe (\c -> getTransfers planCr self c self scoring) otherCountries
    -- Concatenate all the possible operations together
    allOperations = transformOperations ++ transferFromSelfOperations ++ transferToSelfOperations
    -- Converts a generated schedule item into a PlanItem with additional info
    -- needed to carry into the next iteration

-- Converts a generated schedule item into a PlanItem and computes the expected
-- utility for the schedule item using the original score, the discounted
-- reward, and the schedule probability
makePlanItem :: CountryResources -> PlannerConfig -> Double -> Int -> Double -> Double -> [ScheduleItem] -> ScheduleItem -> PlanItem
makePlanItem cr pc@(PlannerConfig self otherCountries depthBound frontierMaxSize numSchedules gamma scoring transforms) baseScore currDepth currGamma currP currSched (ScheduleItem op score) =
  PlanItem (currDepth+1) ((score-baseScore)*currGamma*gamma) (currGamma*gamma) newP (currSched ++ [ScheduleItem op eu]) (applyOp cr op 1)
  where
    dr = (score-baseScore)*currGamma*gamma
    newP = currP * computeP cr self scoring op
    -- Use a hard-coded penalty of -1.0 for a failute
    -- There are probably no failures right now because transfers are
    -- computed in a way that benefits everyone
    eu = (newP * dr) + (1.0 - newP) * (-10.0)

acceptSchedule :: CountryResources -> PlannerConfig -> [ScheduleItem] -> Bool
acceptSchedule cr (PlannerConfig self _ _ _ _ _ scoring _) schedule =
  finalScore - initialScore >= 0
  where
    initialScore = computeScore (cr Map.! self) scoring
    finalScore = computeScore (applySchedule cr schedule Map.! self) scoring
  
