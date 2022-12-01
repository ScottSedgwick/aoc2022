module Main (main) where

import Test.Hspec
import Tests.Test01

main :: IO ()
main = hspec $ do
    tests
