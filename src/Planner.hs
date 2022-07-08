module Planner where

import Resources
import Queue
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Map as Map

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

-- Compute a list of schedules for the given country with the
-- specified bounding parameters, transforms, and scoring parameters
computeSchedule :: CountryResources -> String -> Int -> Int -> Int -> [Transform] -> [ScoreParameter] -> [[ScheduleItem]]
computeSchedule cm self depthBound frontierMaxSize numSchedules transforms scoring  =
  bestSchedules
  where
    -- Compute all the schedules
    allSchedules = iterateSchedule cm self otherCountries depthBound transforms scoring initQueue scheduleQueue 0
    -- Convert each PlanItem to a schedule
    bestSchedules = map getPISchedule $ allQueueItems allSchedules
    getPISchedule (PlanItem _ _ s _) = s
    -- Create the frontier
    queue = createQueue frontierMaxSize
    -- Create a bounded priority queue to hold the resulting schedules
    scheduleQueue = createQueue numSchedules
    -- Get the initial set of possible moves
    moves = getMoves self otherCountries transforms scoring (PlanItem 0 0.0 [] cm)
    -- Add the initial set of moves to the work queue (if we start with an empty
    -- work queue there would be nothing to do)
    initQueue = foldl' addItem queue moves
    -- The other countries are all the countries except self
    otherCountries = delete self $ Map.keys cm

-- Gets the next plan state from the queue, and if it hasn't
-- exceeded the depth bounds, finds any moves to make from it
-- While this function is recursive, the recursive calls are
-- all from the tail position so it is equivalent to iteration
iterateSchedule :: CountryResources -> String -> [String] -> Int -> [Transform] -> [ScoreParameter] -> PriorityQueue -> PriorityQueue -> Int -> PriorityQueue
iterateSchedule cm self otherCountries depthBound transforms scoring queue itemQueue iterations =
--  trace ("Next item is "++show nextItem)
  (
-- If the work queue is empty, we are done, show the number of iterations
  if isNothing nextItem then
    trace ("Total iterations: "++show iterations) itemQueue
-- If the current item hits the depth bound, don't compute additional moves
-- just use the currQueue (the one from which this item was removed)
  else if nextDepth >= depthBound then
    iterateSchedule cm self otherCountries depthBound transforms scoring currQueue (addItem itemQueue (fromJust nextItem)) (iterations+1)
-- Otherwise compute the next moves (by referencing nextQueue instead of currQueue
-- in the recursive call)  
  else
    iterateSchedule cm self otherCountries depthBound transforms scoring nextQueue (addItem itemQueue (fromJust nextItem)) (iterations+1)
  )
  where
    -- Get the next item and updated queue
    (nextItem, currQueue) = getNext queue
    -- Extract the item from the Maybe structure
    (PlanItem nextDepth nextPriority nextSchedule _) = fromJust nextItem
    -- Compute the possible next moves from the current item
    moves = getMoves self otherCountries transforms scoring (fromJust nextItem)
    -- Add all the newly generated moves to the frontier (queue)
    nextQueue = foldl' addItem currQueue moves

-- Computes the possible moves from a given position    
getMoves :: String -> [String] -> [Transform] -> [ScoreParameter] -> PlanItem -> [PlanItem]
getMoves self otherCountries transforms scoring (PlanItem currDepth _ currSched planCr) =
  -- For each possible schedule item, create a PlanItem containing
  -- the next depth, the score, the schedule, and a snapshot of the resources
  map makePlanItemScore allOperations
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
    makePlanItemScore si@(ScheduleItem op score) = PlanItem (currDepth+1) score (currSched ++ [si]) (applyOp planCr op 1)

