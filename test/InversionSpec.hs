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
      inversions ([5,4,3,2,1], 5) `shouldBe` ([1,2,3,4,5], 10)
      inversions ([6,5,4,3,2,1], 6) `shouldBe` ([1,2,3,4,5,6], 15)
      inversions ([10000,9999..1], 10000) `shouldBe` ([1,2..10000], 49995000)
    it "passes SO examples" $ do
      inversions ([3,1,2], 3) `shouldBe` ([1,2,3], 2)
      inversions ([3,5,1,2], 4) `shouldBe` ([1,2,3,5], 4)
      inversions ([2,1,4,3], 4) `shouldBe` ([1,2,3,4], 2)
      inversions ([3,5,1,2,4], 5) `shouldBe` ([1,2,3,4,5], 5)
      inversions ([4,1,3,2,9,5], 6) `shouldBe` ([1,2,3,4,5,9], 5)
      inversions ([3,4,6,1,2,5], 6) `shouldBe` ([1,2..6], 7)
      inversions ([-1,6,3,4,7,4], 6) `shouldBe` ([-1,3,4,4,6,7], 4)
    it "coursera test cases" $ do
      snd (inversions ([4, 7, 2, 1, 9, 10, 6, 3, 5, 8], 10)) `shouldBe` 19
      snd (inversions ([9, 12, 3, 1, 6, 8, 2, 5, 14, 13, 11, 7, 10, 4, 0], 15)) `shouldBe` 56
      snd (inversions ([4, 80, 70, 23, 9, 60, 68, 27, 66, 78, 12, 40, 52, 53, 44, 8, 49, 28, 18, 46, 21, 39, 51, 7, 87, 99, 69, 62, 84, 6, 79, 67, 14, 98, 83, 0, 96, 5, 82, 10, 26, 48, 3, 2, 15, 92, 11, 55, 63, 97, 43, 45, 81, 42, 95, 20, 25, 74, 24, 72, 91, 35, 86, 19, 75, 58, 71, 47, 76, 59, 64, 93, 17, 50, 56, 94, 90, 89, 32, 37, 34, 65, 1, 73, 41, 36, 57, 77, 30, 22, 13, 29, 38, 16, 88, 61, 31, 85, 33, 54], 100)) `shouldBe` 2372
      snd (inversions ([37, 7, 2, 14, 35, 47, 10, 24, 44, 17, 34, 11, 16, 48, 1, 39, 6, 33, 43, 26, 40, 4, 28, 5, 38, 41, 42, 12, 13, 21, 29, 18, 3, 19, 0, 32, 46, 27, 31, 25, 15, 36, 20, 8, 9, 49, 22, 23, 30, 45], 50)) `shouldBe` 490
