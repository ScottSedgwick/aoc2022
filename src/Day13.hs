module Day13 (Input, datafile, parser, part1, part2) where

import Algorithm.Search ( dijkstra )
import qualified Data.Attoparsec.Text as A
import Data.Char ( ord )
import qualified Data.Matrix as M
import Data.List ( elemIndex ) 
import ParserUtils ( strLine ) 

type Input = Int

datafile :: FilePath
datafile = "data/Day13.txt"

-- ################################################################################

parser :: A.Parser Input
parser = pure 0

-- ################################################################################

part1 :: Input -> Int
part1 xs = 0

-- ################################################################################

part2 :: Input -> Int
part2 xs = 0