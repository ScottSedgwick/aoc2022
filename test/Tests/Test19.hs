module Tests.Test19
  ( module Day19
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day19 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 19 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 1264
    it "part2" $ do
      part2 ys `shouldBe` 13475