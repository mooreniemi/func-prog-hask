module Inversion where
-- https://www.coursera.org/learn/algorithm-design-analysis/lecture/GFmmJ/o-n-log-n-algorithm-for-counting-inversions-i

countSplitInv :: (Ord a) => [a] -> Int -> [a] -> ([a], Int) -> ([a], Int)
countSplitInv l left_length r (acc,n)
  | null l || null r = ((reverse acc) ++ l ++ r, n)
  | otherwise = case (l,r) of
                  (x:xs,y:ys) -> if x > y
                                 then countSplitInv l left_length ys (y:acc, n+left_length)
                                 else countSplitInv xs (left_length-1) r (x:acc, n)

inversions :: (Ord a) => ([a], Int) -> ([a], Int)
inversions ([], 0) = ([], 0)
inversions (a,n)
  | n == 1 = (a,0)
  | otherwise = (d, x+y+z)
  where
    left_length = n `div` 2
    right_length = n - left_length
    (l,r) = splitAt left_length a
    (b,x) = inversions (l, left_length)
    (c,y) = inversions (r, right_length)
    (d,z) = countSplitInv b left_length c ([],0)
