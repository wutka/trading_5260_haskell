module Planner where

import Resources
import Queue

--def my_country_scheduler (your_country_name, resources_filename,
--                         initial_state_filename,  output_schedule_filename,
--                         num_output_schedules, depth_bound, frontier_max_size)

computeSchedule :: CountryResources -> String -> Int -> Int -> Int -> [[ScheduleItem]]
computeSchedule cm self numOutputSchedules depthBound frontierMaxSize =

iterateSchedule :: CountryResources -> String -> Int -> Int -> Int -> PriorityQueue -> [[ScheduleItem]] -> [[ScheduleItem]]
iterateSchedule cm self numOutputSchedules depthBound frontierMaxSize queue itemList =
  
  

