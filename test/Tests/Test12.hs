module Tests.Test12
  ( module Day12
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day12 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 12 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 440
    it "part2" $ do
      part2 ys `shouldBe` 439