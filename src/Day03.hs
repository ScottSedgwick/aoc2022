module Day03 (Input, datafile, parser, part1, part2) where

-- import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import ParserUtils

type Input = [Int]

datafile :: FilePath
datafile = "data/Day03.txt"

parser :: A.Parser Input
parser = A.many1 $ intLine

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined