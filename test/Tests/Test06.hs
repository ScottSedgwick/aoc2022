module Tests.Test06
  ( module Day06
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day06 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 06 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` (Just 1892)
    it "part2" $ do
      part2 ys `shouldBe` (Just 2313)