module Main (main) where

import qualified Text.Trifecta as A
import Test.Hspec (Spec, hspec)
import ParserUtils (prtParserError)

import qualified Tests.Test01 as T01
import qualified Tests.Test01 as T02
import qualified Tests.Test01 as T03
import qualified Tests.Test01 as T04
import qualified Tests.Test01 as T05
import qualified Tests.Test01 as T06
import qualified Tests.Test01 as T07
import qualified Tests.Test01 as T08
import qualified Tests.Test01 as T09
import qualified Tests.Test01 as T10
import qualified Tests.Test01 as T11
import qualified Tests.Test01 as T12
import qualified Tests.Test01 as T13
import qualified Tests.Test01 as T14
import qualified Tests.Test01 as T15
import qualified Tests.Test01 as T16
import qualified Tests.Test01 as T17
import qualified Tests.Test01 as T18
import qualified Tests.Test01 as T19
import qualified Tests.Test01 as T20
import qualified Tests.Test01 as T21
import qualified Tests.Test01 as T22
import qualified Tests.Test01 as T23
import qualified Tests.Test24 as T24 
import qualified Tests.Test24 as T25

main :: IO ()
main = do
    loadData T01.datafile T01.parser >>= runTest T01.tests
    loadData T02.datafile T02.parser >>= runTest T02.tests
    loadData T03.datafile T03.parser >>= runTest T03.tests
    loadData T04.datafile T04.parser >>= runTest T04.tests
    loadData T05.datafile T05.parser >>= runTest T05.tests
    loadData T06.datafile T06.parser >>= runTest T06.tests
    loadData T07.datafile T07.parser >>= runTest T07.tests
    loadData T08.datafile T08.parser >>= runTest T08.tests
    loadData T09.datafile T09.parser >>= runTest T09.tests
    loadData T10.datafile T10.parser >>= runTest T10.tests
    loadData T11.datafile T11.parser >>= runTest T11.tests
    loadData T12.datafile T12.parser >>= runTest T12.tests
    loadData T13.datafile T13.parser >>= runTest T13.tests
    loadData T14.datafile T14.parser >>= runTest T14.tests
    loadData T15.datafile T15.parser >>= runTest T15.tests
    loadData T16.datafile T16.parser >>= runTest T16.tests
    loadData T17.datafile T17.parser >>= runTest T17.tests
    loadData T18.datafile T18.parser >>= runTest T18.tests
    loadData T19.datafile T19.parser >>= runTest T19.tests
    loadData T20.datafile T20.parser >>= runTest T20.tests
    loadData T21.datafile T21.parser >>= runTest T21.tests
    loadData T22.datafile T22.parser >>= runTest T22.tests
    loadData T23.datafile T23.parser >>= runTest T23.tests
    loadData T24.datafile T24.parser >>= runTest T24.tests
    loadData T25.datafile T25.parser >>= runTest T25.tests
    
loadData :: FilePath -> (A.Parser a) -> IO  (A.Result a)
loadData datafile parser = do
    xs <- readFile datafile
    pure $ A.parseString parser mempty xs

runTest :: (a -> Spec) -> A.Result a -> IO()
runTest tests xs = do
    case xs of
        A.Success a -> hspec (tests a)
        A.Failure b -> prtParserError b
