module Queue where

import
import Data.PQueue.Max as PQMax

data PQItem = PQItem Double a

instance Eq (PQItem a) where
  (==) (PQItem m _) (PQItem n _) = m == n

instance Ord (PQItem a) where
  compare (PQItem m _) (PQItem n _) = compare m n
  

data PriorityQueue a = PriorityQueue Integer (PQMax.MaxQueue (PQItem a))




