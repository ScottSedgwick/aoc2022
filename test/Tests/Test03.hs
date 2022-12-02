module Tests.Test03
  ( module Day03
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day03 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 02 tests" $ do
    it "part1" $ part1 ys `shouldBe` 14375
    it "part2" $ part2 ys `shouldBe` 10274