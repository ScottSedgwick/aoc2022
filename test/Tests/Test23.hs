module Tests.Test23
  ( module Day23
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day23 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 23 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 3864
    it "part2" $ do
      part2 ys `shouldBe` 946