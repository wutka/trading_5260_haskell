module SkewHeap where

-- This implementation of a Skew Heap (Sleator & Tarjan) comes from
-- Louis Wasserman's "Playing with Priority Queues" in
-- The Monad Reader #16
-- There may be faster Haskell priority queue implementations,
-- but this one is just so simple

data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 <= x2    = SkewNode x1 (heap2 +++ r1) l1 
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

extractMin Empty = Nothing
extractMin (SkewNode x l r ) = Just (x , l +++ r )

singleton :: Ord a => a -> SkewHeap a
singleton x = SkewNode x Empty Empty
