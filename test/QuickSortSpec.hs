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
  describe "quicksort" $ do
    it "handles empty sequences" $ do
      let emptyIntSeq = S.fromList([] :: [Integer])
      quicksort emptyIntSeq `shouldBe` emptyIntSeq
