module Tests.Test11
  ( module Day11
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day11 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 11 tests" $ do
    it "part1" $ do
      part1 ys  `shouldBe` 119715
    it "part2" $ do
<<<<<<< HEAD
      part2 ys `shouldBe` 18085004878
=======
      part2 ys `shouldBe` 18085004878
>>>>>>> d0d7fa7 (Completed Day 11.)
