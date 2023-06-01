module Tests.Test15
  ( module Day15
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day15 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 15 tests" $ do
    -- it "part1" $ do
    --   part1 ys `shouldBe` 0
    it "part2" $ do
      part2 ys `shouldBe` 11318723411840