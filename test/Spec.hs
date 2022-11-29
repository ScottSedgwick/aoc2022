module Main (main) where

import Test.Hspec
import Tests.Day01

main :: IO ()
main = hspec $ do
    tests
