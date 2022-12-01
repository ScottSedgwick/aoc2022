module Tests.Test02 
  ( module Day02
  , tests
  ) where

import Test.Hspec
import Day02

tests :: Input -> Spec
tests ys = describe "Day 02 tests" $ do
    it "part1" $
        part1 ys == 24000
    it "part2" $
        part2 ys == 41000