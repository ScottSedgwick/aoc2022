module Tests.Test17
  ( module Day17
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day17 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 17 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 3147
    it "part2" $ do
      part2 ys `shouldBe` 1532163742758