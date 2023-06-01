module Tests.Test20
  ( module Day20
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day20 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 20 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 988
    it "part2" $ do
      part2 ys `shouldBe` 7768531372516