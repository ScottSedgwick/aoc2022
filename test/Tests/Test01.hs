module Tests.Test01 
  ( module Day01
  , tests
  ) where

import Test.Hspec
import Day01

tests :: Input -> Spec
tests ys = describe "Day 01 tests" $ do
    it "part1" $
        part1 ys == 69289
    it "part2" $
        part2 ys == 205615