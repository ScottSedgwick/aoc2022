module Tests.Test13
  ( module Day13
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day13 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 13 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 0
    it "part2" $ do
      part2 ys `shouldBe` 0