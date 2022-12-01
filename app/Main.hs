module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Day01

main :: IO ()
main = do
    xs <- T.readFile datafile
    case A.parseOnly parser xs of
        (Left s) -> print s
        (Right ys) -> do
            let x = part1 ys
            putStr "Part One: "
            print x
            
            let y = part2 ys
            putStr "Part Two: "
            print y
