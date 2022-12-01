module Tests.Test02 
  ( module Day02
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day02 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 02 tests" $ do
    it "part1" $ part1 ys `shouldBe` 24000
    it "part2" $ part2 ys `shouldBe` 41000