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
part1 = p 4 0 

p :: Int -> Int -> String -> Maybe Int
p _ _ [] = Nothing
p l n xs = if length xs < l
           then Nothing
           else if (length (nub (take l xs)) == l)
                then Just (n + l)
                else p l (n + 1) (tail xs)

part2 :: Input -> Maybe Int
part2 = p 14 0 
