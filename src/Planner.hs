module Planner where

import Resources
import Queue
import Data.Maybe
import Data.List
import Debug.Trace

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

computeSchedule :: CountryResources -> String -> Int -> Int -> [Transform] -> [ScoreParameter] -> [[ScheduleItem]]
computeSchedule cm self depthBound frontierMaxSize transforms scoring  =
  iterateSchedule cm self depthBound transforms scoring initQueue []
  where
    queue = createQueue frontierMaxSize
    moves = getMoves self transforms scoring (PlanItem 0 [] cm)
    initQueue = foldl' addNextItem queue moves

addNextItem :: PriorityQueue -> (PlanItem,Double) -> PriorityQueue
addNextItem queue (item,score) = addItem queue item score

iterateSchedule :: CountryResources -> String -> Int -> [Transform] -> [ScoreParameter] -> PriorityQueue -> [[ScheduleItem]] -> [[ScheduleItem]]
iterateSchedule cm self depthBound transforms scoring queue itemList =
  trace ("Next item: "++show nextItem)
  (if isNothing nextItem then
    itemList
  else if nextDepth >= depthBound then
    iterateSchedule cm self depthBound transforms scoring currQueue (nextSchedule:itemList)
  else 
    iterateSchedule cm self depthBound transforms scoring nextQueue itemList)
  where
    (nextItem, currQueue) = getNext queue    
    (PlanItem nextDepth nextSchedule _) = fromJust nextItem
    moves = getMoves self transforms scoring (fromJust nextItem)
    nextQueue = foldl' addNextItem currQueue moves
    
getMoves :: String -> [Transform] -> [ScoreParameter] -> PlanItem -> [(PlanItem,Double)]
getMoves self transforms scoring (PlanItem currDepth currSched planCr) =
  map makePlanItemScore allOperations
  where
    selfTransformOps = map (createTransformOp self 1) transforms
    allOperations = concatMap (bestOperationQuantities planCr self scoring) selfTransformOps
    makePlanItemScore si@(ScheduleItem op score) = (PlanItem (currDepth+1) (currSched ++ [si]) (applyOp planCr op 1),
                                                    score)

