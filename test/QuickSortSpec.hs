{-# LANGUAGE ScopedTypeVariables #-}
module QuickSortSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import QuickSort
import qualified Data.Sequence as S
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "partition" $ do
    it "single element is vacuously parititioned" $ do
      partition'(S.fromList([3])) `shouldBe` S.fromList([3])
    it "double element sorts" $ do
      partition'(S.fromList([3,2])) `shouldBe` S.fromList([2,3])
      partition'(S.fromList([2,3])) `shouldBe` S.fromList([2,3])
    it "otherwise continues scanning" $ do
      partition'(S.fromList([3,2,1])) `shouldBe` S.fromList([2,1,3])
      partition'(S.fromList([3,4,2,1])) `shouldBe` S.fromList([1,2,3,4])
    it "should place pivot element at its sorted position" $ property $
      \(NonEmpty s@((x :: Int):_)) -> let sorted = sort s
                                          correct_position = elemIndex x sorted
                                          partitioned_index = S.elemIndexL x (partition'(S.fromList s))
                                      in
                                        partitioned_index === correct_position
    it "all left of pivot smaller than pivot, all right larger than pivot" $ property $
      \(NonEmpty s@((x :: Int):_)) -> let partitioned_seq = partition'(S.fromList s)
                                          partitioned_index = fromJust $ S.elemIndexL x (partitioned_seq)
                                          (firstHalf,secondHalf) = S.splitAt partitioned_index partitioned_seq
                                      in
                                        all (< x) firstHalf && all (>= x) secondHalf
