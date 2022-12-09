module Tests.Test08
  ( module Day08
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day08 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 08 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 1538
    it "part2" $ do
      part2 ys `shouldBe` 496125