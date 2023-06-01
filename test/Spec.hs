module Main (main) where

import qualified Data.Text.IO as T
import qualified Text.Trifecta as A
import Test.Hspec (hspec)
import ParserUtils (prtParserError, pEither)

import Tests.Test24 (datafile, parser, tests)

main :: IO ()
main = readFile datafile >>= pEither (hspec . tests) prtParserError . A.parseString parser mempty
