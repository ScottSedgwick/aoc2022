module Tests.Test06
  ( module Day06
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day06 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 06 tests" $ do
    undefined
    -- it "parser" $ stacks ys `shouldBe` M.fromList [(1,"NHSJFWTD"),(2,"GBNTQPRH"),(3,"VQL"),(4,"QRWSBN"),(5,"BMVTFDN"),(6,"RTHVBDM"),(7,"JQBD"),(8,"QHZRVJND"),(9,"SMHNB")]
    -- it "part1" $ do
    --   part1 ys `shouldBe` "GRTSWNJHH"
    -- it "part2" $ do
    --   part2 ys `shouldBe` "QLFQDBBHM"