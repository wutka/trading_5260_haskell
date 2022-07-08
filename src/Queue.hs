module Queue where

import Resources

import Data.Maybe
import Data.PSQueue
import Debug.Trace

-- An item in the PQ has a numeric key and a value
data PQItem = PQItem Int PlanItem

-- Use two queues with ascending and descending scores
data PriorityQueue = PriorityQueue Int Int (PSQ PQItem Double) (PSQ PQItem Double)

-- To compare two PQItems, just compare their keys
instance Eq PQItem where
  (==) (PQItem key1 _) (PQItem key2 _) = key1 == key2

-- To sort two PQ items, just compare their keys
instance Ord PQItem where
  compare (PQItem key1 _) (PQItem key2 _) = compare key1 key2

-- Creates a new, empty bounded priority queue
createQueue :: Int -> PriorityQueue
createQueue n = PriorityQueue n 0 empty empty

-- Gets the next item from the queue and returns both the item
-- and a copy of the queue with the item removed
getNext :: PriorityQueue -> (Maybe PlanItem, PriorityQueue)
getNext (PriorityQueue maxSize nextKey maxQueue minQueue) =
  -- get the min value from the max queue (i.e. the best score)
  -- and remove it from both the ascending and descending queues
  getQueueFromValue $ findMin maxQueue
  where
    getQueueFromValue Nothing = (Nothing, PriorityQueue maxSize nextKey empty empty)
    -- Use deleteMin to delete the found item, and then use its key to
    -- delete it from the other queue
    getQueueFromValue (Just (item@(PQItem key v) :-> _)) =
      (Just v, PriorityQueue maxSize nextKey (deleteMin maxQueue) (delete item minQueue))

-- Adds an item to the queue
addItem :: PriorityQueue -> PlanItem -> PriorityQueue
addItem (PriorityQueue maxSize currKey maxQueue minQueue) val@(PlanItem _ priority _ _) =
  -- Clean the queue, make sure it doesn't exceed the bounds
  cleanQueue $
  -- Create a new PQ instance adding the item with its negative score (since PSQ only
  -- looks for minumum values) and to the minQueue (lowest score) queue
  -- Assign a key to the current value and increment the next key value
  PriorityQueue maxSize (currKey+1) (insert (PQItem currKey val) (-1.0*priority) maxQueue)
                                    (insert (PQItem currKey val) priority minQueue)

cleanQueue :: PriorityQueue -> PriorityQueue
cleanQueue pq@(PriorityQueue maxSize nextKey maxQueue minQueue) =
--  trace ("Queue size: "++show (size maxQueue))
  (
-- If the queue doesn't exceed the bounds, just return it
  if size maxQueue <= maxSize then
    pq
  else
-- Find the item with the lowest score (findMin on minQueue) and then
-- delete it with deleteMin from the min queue, and delete it by its key
-- from the maxQueue
    PriorityQueue maxSize nextKey (delete item maxQueue) (deleteMin minQueue)
  )
  where
    (item :-> _) = fromJust (findMin minQueue)

-- Returns all the PlanItems in the queue in order from highest to lowest
allQueueItems :: PriorityQueue -> [PlanItem]
allQueueItems (PriorityQueue _ _ maxQueue _) =
  map getPlanItemFromKey $ keys maxQueue
  where
    getPlanItemFromKey (PQItem _ pi) = pi

  

