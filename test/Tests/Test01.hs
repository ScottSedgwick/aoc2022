module Tests.Test01 
  ( module Day01
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day01 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 01 tests" $ do
    it "part1" $ part1 ys `shouldBe` 69289
    it "part2" $ part2 ys `shouldBe` 205615