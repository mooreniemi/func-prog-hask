module QuickSort where
import Data.Sequence as S
import Data.Foldable as F

quicksort :: (Ord a) => S.Seq a -> S.Seq a
quicksort a
  | n <= 1 = a
  where
    n = S.length a
