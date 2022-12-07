module Day06 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A
import Data.List (nub)
import ParserUtils ( strLine )

type Input = String

datafile :: FilePath
datafile = "data/Day06.txt"

parser :: A.Parser Input
parser = strLine

part1 :: Input -> Maybe Int
part1 = f 4 0 

part2 :: Input -> Maybe Int
part2 = f 14 0 

f :: Int -> Int -> String -> Maybe Int
f l n xs | length xs < l                 = Nothing
         | length (nub (take l xs)) == l = Just (n + l)
         | otherwise                     = f l (n + 1) (tail xs)
