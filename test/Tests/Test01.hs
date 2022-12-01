module Tests.Test01 (tests) where

import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Test.Hspec
import Day01

tests :: Spec
tests = do
    xs <- runIO (T.readFile datafile)
    case A.parseOnly parser xs of
        (Left s) -> describe "parser failure" $ it s $ False
        (Right ys) -> do
            describe "Day 01 tests" $ do
                it "part1" $
                    part1 ys == 1564
                it "part2" $
                    part2 ys == 1611