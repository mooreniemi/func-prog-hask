module QuickSortSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import QuickSort
import Data.Sequence as S

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "partition" $ do
    it "single element is vacuously parititioned" $ do
      partition'(S.fromList([3])) `shouldBe` S.fromList([3])
    it "returns seq when seen boundary exceeds length" $ do
      partition'(S.fromList([3,2])) `shouldBe` S.fromList([2,3])
    it "otherwise continues scanning" $ do
      partition'(S.fromList([3,2,1])) `shouldBe` S.fromList([2,1,3])
  describe "quicksort" $ do
    it "handles empty sequences" $ do
      let emptyIntSeq = S.fromList([] :: [Integer])
      quicksort emptyIntSeq `shouldBe` emptyIntSeq
