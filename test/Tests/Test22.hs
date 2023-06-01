module Tests.Test22
  ( module Day22
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day22 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 22 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 0
    it "part2" $ do
      part2 ys `shouldBe` 0