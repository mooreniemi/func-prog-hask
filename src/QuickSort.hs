module QuickSort where
import Data.Sequence as S
import Data.Foldable as F

partition' :: (Ord a) => S.Seq a -> S.Seq a
partition' seq = (firstHalf|>pivot)><secondHalf
  where
    (_, position) = partitioner seq 0 0
    pivot = S.index seq 0
    (_, rest) = S.splitAt 1 seq
    (firstHalf, secondHalf) = S.splitAt position rest

partitioner :: (Ord a) => S.Seq a -> Int -> Int -> (S.Seq a, Int)
partitioner a i j
  | n <= 1 = (a, 0)
  | j > n = (a, i-2)
  | otherwise = case (viewl a) of
                (p:<ps) -> case (viewl ps) of
                           (x:<xs) -> partitioner a partition_boundary seen_boundary
                                      where
                                        seen_boundary = j+1
                                        partition_boundary = if p>x
                                                             then i+1
                                                             else i
  where
    n = S.length a

quicksort :: (Ord a) => S.Seq a -> S.Seq a
quicksort a
  | n <= 1 = a
  where
    n = S.length a
