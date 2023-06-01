module Main (main) where

import qualified Text.Trifecta as A
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import ParserUtils (prtParserError, pEither)

import Day25 (Input, datafile, parser, part1, part2)

main :: IO ()
main = readFile datafile >>= pEither prtResult prtParserError . A.parseString parser mempty

prtResult :: Input -> IO()
prtResult ys = do
    putStrLn "Data: "
    print ys

    st1 <- getCurrentTime
    putStrLn "Part One: "
    print $ part1 ys
    et1 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et1 st1) <> " secs."
    
    st2 <- getCurrentTime
    putStrLn "Part Two: "
    print $ part2 ys
    et2 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et2 st2) <> " secs."
