module Day01 (datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A

type Input = [Int]

datafile :: FilePath
datafile = "data/Day01.txt"

parser :: A.Parser Input
parser = A.many1 $ do
  x <- A.decimal
  A.endOfLine <|> A.endOfInput
  pure x

part1 :: Input -> Int
part1 (x:y:xs) = (if y > x then 1 else 0) + part1 (y:xs)
part1 _ = 0

part2 :: Input -> Int
part2 (a:b:c:d:xs) = (if s2 > s1 then 1 else 0) + part2 (b:c:d:xs)
  where
    s1 = a + b + c
    s2 = b + c + d
part2 _ = 0