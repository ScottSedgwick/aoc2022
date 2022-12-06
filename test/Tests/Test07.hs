module Tests.Test07
  ( module Day07
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day07 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 07 tests" $ do
    undefined
    -- it "part1" $ do
    --   part1 ys `shouldBe` (Just 1892)
    -- it "part2" $ do
    --   part2 ys `shouldBe` (Just 2313)