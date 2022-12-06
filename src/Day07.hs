module Day07 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A

type Input = String

datafile :: FilePath
datafile = "data/Day07.txt"

parser :: A.Parser Input
parser = undefined

part1 :: Input -> Maybe Int
part1 = undefined

part2 :: Input -> Maybe Int
part2 = undefined 
