-- https://www.coursera.org/learn/algorithm-design-analysis/lecture/GFmmJ/o-n-log-n-algorithm-for-counting-inversions-i
countSplitInv :: [a] -> Int -> Int
countSplitInv a n = n

inversions :: [a] -> Int -> Int
inversions a n
  | n == 1 = 0
  | otherwise = x + y + z
  where
    half_length = n `div` 2
    x = inversions (fst $ splitAt half_length a) half_length
    y = inversions (snd $ splitAt half_length a) half_length
    z = countSplitInv a n
