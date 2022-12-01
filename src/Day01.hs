module Day01 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A
import Data.List (sort)
import ParserUtils

type Input = [[Int]]

datafile :: FilePath
datafile = "data/Day01.txt"

parser :: A.Parser Input
parser = A.many1 intGroup

part1 :: Input -> Int
part1 = maximum . map sum

part2 :: Input -> Int
part2 = sum . take 3 . reverse . sort . map sum