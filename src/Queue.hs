module Queue where

import Resources

import Data.PQueue.Max as PQMax

data PlanItem = PlanItem Int [ScheduleItem]

data PQItem = PQItem Double PlanItem 

instance Eq PQItem where
  (==) (PQItem m _) (PQItem n _) = m == n

instance Ord PQItem where
  compare (PQItem m _) (PQItem n _) = compare m n
  

data PriorityQueue = PriorityQueue Int (PQMax.MaxQueue PQItem)

getNext :: PriorityQueue -> (Maybe PlanItem, PriorityQueue)
getNext (PriorityQueue maxSize queue) =
  (val, PriorityQueue maxSize nextQueue)
  where
    getVal (Just (PQItem _ v)) = Just v
    getVal Nothing = Nothing
    val = getVal $ PQMax.getMax queue
    nextQueue = PQMax.deleteMax queue

addItem :: PriorityQueue -> PlanItem -> Double -> PriorityQueue
addItem (PriorityQueue maxSize queue) val priority =
  PriorityQueue maxSize (PQMax.fromList $ PQMax.take maxSize $ PQMax.insert (PQItem priority val) queue)
  

