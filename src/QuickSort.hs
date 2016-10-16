{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module QuickSort where
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..),ViewL(..),(|>),(><),(<|))
import Control.Arrow as A (first)
import Debug.Trace

-- moves pivot element into correct position
partition' :: (Show a, Ord a) => S.Seq a -> S.Seq a
partition' seq = case first S.viewl (partitioner seq 0 1) of
                   (pivot:<s,position) -> (firstHalf|>pivot)><secondHalf
                     where
                       (firstHalf,secondHalf) = S.splitAt position s

-- tells you where the pivot element's final position should be
partitioner :: (Show a, Ord a) => S.Seq a -> Int -> Int -> (S.Seq a, Int)
partitioner a i j
  | n <= 1 || j > n = (a, i)
  | otherwise = case (S.viewl a) of
                (p:<ps) -> let (seen, unseen) = S.splitAt j ps
                           in case S.viewr seen of
                                (xs:>x) -> partitioner b partition_boundary seen_boundary
                                  where
                                    seen_boundary = j+1
                                    partition_boundary = if x<p
                                                         then i+1
                                                         else i
                                    b = if x<p
                                        then ((p<|(x<|xs))><unseen)
                                        else ((p<|(xs|>x))><unseen)
                                (EmptyR) -> (a, i)
  where
    n = S.length a
