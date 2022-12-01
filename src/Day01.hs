module Day01 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import Data.List (sort)

type Input = [[Int]]

datafile :: FilePath
datafile = "data/Day01.txt"

parser :: A.Parser Input
parser = A.many1 $ do
  xs <- A.many1 $ do
    x <- A.decimal
    A.endOfLine <|> A.endOfInput
    pure x
  A.endOfLine
  pure xs

part1 :: Input -> Int
part1 = maximum . map sum

part2 :: Input -> Int
part2 = sum . take 3 . reverse . sort . map sum