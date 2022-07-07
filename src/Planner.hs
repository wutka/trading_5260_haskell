module Planner where

import Resources
import Queue
import Data.Maybe
import Data.List
import Debug.Trace

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

computeSchedule :: CountryResources -> String -> Int -> Int -> Int -> [Transform] -> [ScoreParameter] -> [[ScheduleItem]]
computeSchedule cm self depthBound frontierMaxSize numSchedules transforms scoring  =
  bestSchedules
  where
    allSchedules = iterateSchedule cm self depthBound transforms scoring initQueue scheduleQueue
    bestSchedules = map getPISchedule $ allQueueItems allSchedules
    getPISchedule (PlanItem _ _ s _) = s
    compareSchedules slist1 slist2 = compare (schedEu $ last slist1) (schedEu $ last slist2)
    schedEu (ScheduleItem _ eu) = eu
    queue = createQueue frontierMaxSize
    scheduleQueue = createQueue numSchedules
    moves = getMoves self transforms scoring (PlanItem 0 0.0 [] cm)
    initQueue = foldl' addNextItem queue moves

addNextItem :: PriorityQueue -> PlanItem -> PriorityQueue
addNextItem queue item = addItem queue item

iterateSchedule :: CountryResources -> String -> Int -> [Transform] -> [ScoreParameter] -> PriorityQueue -> PriorityQueue -> PriorityQueue
iterateSchedule cm self depthBound transforms scoring queue itemQueue =
  if isNothing nextItem then
    itemQueue
  else 
    iterateSchedule cm self depthBound transforms scoring nextQueue (addItem itemQueue (fromJust nextItem))
  where
    (nextItem, currQueue) = getNext queue    
    (PlanItem nextDepth nextPriority nextSchedule _) = fromJust nextItem
    moves = getMoves self transforms scoring (fromJust nextItem)
    nextQueue = foldl' addNextItem currQueue moves
    
getMoves :: String -> [Transform] -> [ScoreParameter] -> PlanItem -> [PlanItem]
getMoves self transforms scoring (PlanItem currDepth _ currSched planCr) =
  map makePlanItemScore allOperations
  where
    selfTransformOps = map (createTransformOp self 1) transforms
    allOperations = concatMap (bestOperationQuantities planCr self scoring) selfTransformOps
    makePlanItemScore si@(ScheduleItem op score) = PlanItem (currDepth+1) score (currSched ++ [si]) (applyOp planCr op 1)

