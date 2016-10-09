module InversionSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Inversion

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inversions" $ do
    it "for total reversed sort order reverses list and count n choose k" $ do
      inversions ([2,1], 2) `shouldBe` ([1,2], 1)
      inversions ([3,2,1], 3) `shouldBe` ([1,2,3], 3)
      inversions ([6,5,4,3,2,1], 6) `shouldBe` ([1,2,3,4,5,6], 15)
      inversions ([10000,9999..1], 10000) `shouldBe` ([1,2..10000], 49995000)
    it "passes SO examples" $ do
      inversions ([3,1,2], 3) `shouldBe` ([1,2,3], 2)
      inversions ([3,5,1,2], 4) `shouldBe` ([1,2,3,5], 4)
      inversions ([3,5,1,2,4], 5) `shouldBe` ([1,2,3,4,5], 5)
      inversions ([4,1,3,2,9,5], 6) `shouldBe` ([1,2,3,4,5,9], 5)
      inversions ([3,4,6,1,2,5], 6) `shouldBe` ([1,2..6], 7)
