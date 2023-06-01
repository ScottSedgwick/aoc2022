module Tests.Test18
  ( module Day18
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day18 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 18 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 3500
    it "part2" $ do
      part2 ys `shouldBe` 2048