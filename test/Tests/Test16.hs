module Tests.Test16
  ( module Day16
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day16 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 16 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 2330
    it "part2" $ do
      part2 ys `shouldBe` 2675