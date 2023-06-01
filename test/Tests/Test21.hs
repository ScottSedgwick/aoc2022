module Tests.Test21
  ( module Day21
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day21 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 21 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 223971851179174
    it "part2" $ do
      part2 ys `shouldBe` 3379022190351