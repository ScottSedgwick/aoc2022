module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Test.Hspec
import Tests.Test01

main :: IO ()
main = do
    xs <- T.readFile datafile
    case A.parseOnly parser xs of
        (Left s) -> putStrLn ("Parser failure: " <> s)
        (Right ys) -> hspec $ tests ys
