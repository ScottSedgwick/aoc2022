module Tests.Test05
  ( module Day05
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.IntMap as M
import Day05 (Input(..), datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 05 tests" $ do
    it "parser" $ stacks ys `shouldBe` M.fromList [(1,"NHSJFWTD"),(2,"GBNTQPRH"),(3,"VQL"),(4,"QRWSBN"),(5,"BMVTFDN"),(6,"RTHVBDM"),(7,"JQBD"),(8,"QHZRVJND"),(9,"SMHNB")]
    it "part1" $ do
      part1 ys `shouldBe` "GRTSWNJHH"
    it "part2" $ do
      part2 ys `shouldBe` "QLFQDBBHM"