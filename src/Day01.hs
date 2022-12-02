module Day01 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A
import Data.List (sortOn)
import Data.Ord (Down(..))
import ParserUtils (intGroup)

type Input = [Int]

datafile :: FilePath
datafile = "data/Day01.txt"

parser :: A.Parser Input
parser = (sortOn Down . map sum) <$> A.many1 intGroup

part1 :: Input -> Int
part1 = head 

part2 :: Input -> Int
part2 = sum . take 3