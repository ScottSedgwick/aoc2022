module Day08 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Matrix as M
import ParserUtils ( digitLine )

type Input = M.Matrix Int

datafile :: FilePath
datafile = "data/Day08.txt"

parser :: A.Parser Input
parser = do
    xs <- A.many1 digitLine
    let nRows = length xs
    let nCols = length (head xs)
    pure $ M.fromList nRows nCols (concat xs)

part1 :: Input -> Int
part1 xs = length ys
  where
    ks = [(x,y) | x <- [1..M.ncols xs], y <- [1..M.nrows xs] ]
    ys = filter (isVisible xs) ks 

isVisible :: Input -> (Int, Int) -> Bool
isVisible xs (x,y) = all (\z -> xs M.! (z,y) < h) [1..(x-1)]
                  || all (\z -> xs M.! (z,y) < h) [(x+1)..M.ncols xs]
                  || all (\z -> xs M.! (x,z) < h) [1..(y-1)]
                  || all (\z -> xs M.! (x,z) < h) [(y+1)..M.nrows xs]
  where
    h = xs M.! (x,y)

part2 :: Input -> Int
part2 xs = maximum ys
  where
    ys =  map (scenic xs) [(x,y) | x <- [1..M.ncols xs], y <- [1..M.nrows xs] ]

scenic :: Input -> (Int, Int) -> Int
scenic xs (x,y) = norths * souths * easts * wests 
  where
    h = xs M.! (x,y)
    norths = length (takeWhileInclusive (\p -> xs M.! (p,y) < h) (reverse [1..(x-1)])) 
    souths = length (takeWhileInclusive (\p -> xs M.! (p,y) < h) [(x+1)..M.ncols xs]) 
    wests  = length (takeWhileInclusive (\p -> xs M.! (x,p) < h) (reverse [1..(y-1)])) 
    easts  = length (takeWhileInclusive (\p -> xs M.! (x,p) < h) [(y+1)..M.nrows xs]) 

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []     = []
takeWhileInclusive p (x:xs) = x : if p x 
                                  then takeWhileInclusive p xs
                                  else []

