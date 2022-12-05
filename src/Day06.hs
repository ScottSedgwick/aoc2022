module Day06 (Input, datafile, parser, part1, part2) where

-- import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A

type Input = Int

datafile :: FilePath
datafile = "data/Day06.txt"

parser :: A.Parser Input
parser = undefined

part1 :: Input -> String
part1 = undefined

part2 :: Input -> String
part2 = undefined
