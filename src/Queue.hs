module Queue where

import Resources

import Data.Maybe
import Data.PSQueue

data PQItem = PQItem Int PlanItem

data PriorityQueue = PriorityQueue Int Int (PSQ PQItem Double) (PSQ PQItem Double)

instance Eq PQItem where
  (==) (PQItem key1 _) (PQItem key2 _) = key1 == key2

instance Ord PQItem where
  compare (PQItem key1 _) (PQItem key2 _) = compare key1 key2

createQueue :: Int -> PriorityQueue
createQueue n = PriorityQueue n 0 empty empty

getNext :: PriorityQueue -> (Maybe PlanItem, PriorityQueue)
getNext (PriorityQueue maxSize nextKey maxQueue minQueue) =
  getQueueFromValue $ findMin maxQueue
  where
    getQueueFromValue Nothing = (Nothing, PriorityQueue maxSize nextKey empty empty)
    getQueueFromValue (Just (item@(PQItem key v) :-> _)) =
      (Just v, PriorityQueue maxSize nextKey (deleteMin maxQueue) (delete item minQueue))

addItem :: PriorityQueue -> PlanItem -> PriorityQueue
addItem (PriorityQueue maxSize currKey maxQueue minQueue) val@(PlanItem _ priority _ _) =
  cleanQueue $
  PriorityQueue maxSize (currKey+1) (insert (PQItem currKey val) (-1.0*priority) maxQueue)
                                    (insert (PQItem currKey val) priority minQueue)

cleanQueue :: PriorityQueue -> PriorityQueue
cleanQueue pq@(PriorityQueue maxSize nextKey maxQueue minQueue) =
  if size maxQueue <= maxSize then
    pq
  else
    PriorityQueue maxSize nextKey (delete item maxQueue) (deleteMin minQueue)
  where
    (item :-> _) = fromJust (findMin minQueue)

allQueueItems :: PriorityQueue -> [PlanItem]
allQueueItems (PriorityQueue _ _ maxQueue _) =
  map getPlanItemFromKey $ keys maxQueue
  where
    getPlanItemFromKey (PQItem _ pi) = pi

  

