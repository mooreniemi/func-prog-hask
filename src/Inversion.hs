-- https://www.coursera.org/learn/algorithm-design-analysis/lecture/GFmmJ/o-n-log-n-algorithm-for-counting-inversions-i
module Inversion where
import Data.Sequence as S
import Data.Foldable as F

countSplitInv :: (Ord a) => S.Seq a -> S.Seq a -> (S.Seq a, Int) -> (S.Seq a, Int)
countSplitInv l r (acc,n)
  = case (viewl l, viewl r) of
    (EmptyL,_) -> (acc >< r, n)
    (_,EmptyL) -> (acc >< l, n)
    (x:<xs,y:<ys) -> if x > y
      then countSplitInv l ys (acc|>y, n + S.length l)
      else countSplitInv xs r (acc|>x, n)

inversions' :: (Ord a) => S.Seq a -> (S.Seq a, Int)
inversions' a
  | n <= 1 = (a, 0)
  | otherwise = (d, x+y+z)
  where
    n = S.length a
    (l,r) = S.splitAt (n `div` 2) a
    (b,x) = inversions' l
    (c,y) = inversions' r
    (d,z) = countSplitInv b c (S.empty,0)

inversions :: (Ord a) => ([a], Int) -> ([a], Int)
inversions (a,_) = let (list,count) = inversions' $ S.fromList a
                       in (F.toList list, count)
