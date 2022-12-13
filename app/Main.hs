module Main (main) where

import Data.Foldable
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import ParserUtils (prtParserError)

import Day13 (Input, datafile, parser, part1, part2, printPacket)

main :: IO ()
main = T.readFile datafile >>= either prtParserError prtResult . A.parseOnly parser

prtResult :: Input -> IO()
prtResult ys = do
    putStrLn "Data: "
    forM_ ys $ \(a,b) -> do
                        putStrLn (printPacket a)
                        putStrLn (printPacket b)
                        putStrLn ""

    putStrLn "Part One: "
    print $ part1 ys
    
    putStrLn "Part Two: "
    print $ part2 ys
