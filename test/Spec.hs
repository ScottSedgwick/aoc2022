module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Test.Hspec
import ParserUtils (prtParserError)

import Tests.Test02

main :: IO ()
main = T.readFile datafile >>= either prtParserError (hspec . tests) . A.parseOnly parser
