module Planner where

import Resources
import Queue
import Data.Maybe
import Data.List

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

computeSchedule :: CountryResources -> String -> Int -> Int -> Int -> [[ScheduleItem]]
computeSchedule cm self depthBound frontierMaxSize = undefined

iterateSchedule :: CountryResources -> String -> Int -> [Transform] -> [ScoreParameter] -> PriorityQueue -> [[ScheduleItem]] -> [[ScheduleItem]]
iterateSchedule cm self depthBound transforms scoring queue itemList =
  if isNothing nextItem then
    itemList
  else if nextDepth >= depthBound then
    iterateSchedule cm self depthBound transforms scoring nextQueue (nextSchedule:itemList)
  else 
    iterateSchedule cm self depthBound transforms scoring nextQueue itemList
  where
    (nextItem, currQueue) = getNext queue    
    (PlanItem nextDepth nextSchedule) = fromJust nextItem
    moves = getMoves cm self transforms scoring (fromJust nextItem)
    nextQueue = foldl' addNextItem currQueue moves
    addNextItem queue (item,score) = addItem queue item score
    
getMoves :: CountryResources -> String -> [Transform] -> [ScoreParameter] -> PlanItem -> [(PlanItem,Double)]
getMoves cr self transforms scoring (PlanItem currDepth currSched) =
  map makePlanItemScore allOperations
  where
    selfTransformOps = map (createTransformOp self 1) transforms
    allOperations = concatMap (bestOperationQuantities cr self scoring) selfTransformOps
    makePlanItemScore si@(ScheduleItem op score) = (PlanItem (currDepth+1) (currSched ++ [si]),
                                                    score)

