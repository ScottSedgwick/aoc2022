module Tests.Test09
  ( module Day09
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day09 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 09 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 6087
    it "part2" $ do
      part2 ys `shouldBe` 2493