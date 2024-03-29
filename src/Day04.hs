module Day04 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A

type InputPart = ((Integer, Integer), (Integer, Integer))
type Input = [InputPart]

datafile :: FilePath
datafile = "data/Day04.txt"

parser :: A.Parser Input
parser = A.many $ do
    a <- A.decimal
    _ <- A.char '-'
    b <- A.decimal
    _ <- A.char ','
    c <- A.decimal
    _ <- A.char '-'
    d <- A.decimal
    _ <- A.restOfLine
    pure ((a,b), (c,d))


part1 :: Input -> Int
part1 = length . filter contained 

contained :: InputPart -> Bool
contained ((a,b),(c,d)) =  (a<=c && b>=d) 
                        || (a>=c && b<=d)

part2 :: Input -> Int
part2 = length . filter overlap

overlap :: InputPart -> Bool
overlap ((a,b),(c,d)) =  (a >= c && a <= d) 
                      || (a <= c && b >= c) 
                      || (b >= c && b <= d) 
                      || (a <= d && b >= d)