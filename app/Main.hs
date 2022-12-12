module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import ParserUtils (prtParserError)

import Day12 (Input, datafile, parser, part1, part2)

main :: IO ()
main = T.readFile datafile >>= either prtParserError prtResult . A.parseOnly parser

prtResult :: Input -> IO()
prtResult ys = do
    putStrLn "Data: "
    print ys

    putStrLn "Part One: "
    print $ part1 ys
    
    putStrLn "Part Two: "
    print $ part2 ys
