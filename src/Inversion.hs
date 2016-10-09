module Inversion where
-- https://www.coursera.org/learn/algorithm-design-analysis/lecture/GFmmJ/o-n-log-n-algorithm-for-counting-inversions-i

countSplitInv :: (Ord a) => [a] -> [a] -> ([a], Int) -> ([a], Int)
countSplitInv l r (acc,n)
  | null l || null r = (acc ++ l ++ r, n)
  | otherwise = case (l,r) of
                  (x:xs,y:ys) -> if x > y
                                 then countSplitInv l ys (acc ++ [y], n+length l)
                                 else countSplitInv xs r (acc ++ [x], n) 

inversions :: (Ord a) => ([a], Int) -> ([a], Int)
inversions (a,n)
  | n == 1 = (a, 0)
  | otherwise = (d, x+y+z)
  where
    half_length = n `div` 2
    (b,x) = inversions (fst $ splitAt half_length a, half_length)
    (c,y) = inversions (snd $ splitAt half_length a, half_length)
    (d,z) = countSplitInv b c ([],0)
