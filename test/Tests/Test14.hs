module Tests.Test14
  ( module Day14
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day14 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 14 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 655
    it "part2" $ do
      part2 ys `shouldBe` 26484