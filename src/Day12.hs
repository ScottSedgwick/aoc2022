module Day12 (Input, datafile, parser, part1, part2) where

import Algorithm.Search ( dijkstra )
import qualified Text.Trifecta as A
import Data.Char ( ord )
import qualified Data.Matrix as M
import Data.List ( elemIndex ) 
import ParserUtils ( strLine ) 

data Input = Input
  { elevations :: M.Matrix Int
  , pos :: (Int, Int)
  , endPos :: (Int, Int)
  } deriving stock (Show, Eq)

instance Ord Input where
    compare xs ys = compare (pos xs) (pos ys)

datafile :: FilePath
datafile = "data/Day12.txt"

-- ################################################################################

parser :: A.Parser Input
parser = do
    xs <- A.many strLine
    let m = M.fromList (length xs) (length (head xs)) (map toHeight $ concat xs)
    let sp = getPos 'S' xs
    let ep = getPos 'E' xs
    pure $ Input { elevations = m, pos = sp, endPos = ep } 

toHeight :: Char -> Int
toHeight 'S' = toHeight 'a'
toHeight 'E' = toHeight 'z'
toHeight c   = ord c - ord 'a' + 1

getPos :: Char -> [String] -> (Int, Int)
getPos c xs = gp 1 xs
  where
    gp _ []     = (-1, -1)
    gp i (y:ys) = case elemIndex c y of
                        Nothing -> gp (i + 1) ys
                        Just j  -> (i, j + 1)

-- ################################################################################

part1 :: Input -> Int
part1 xs = getCost xs
    
getCost :: Input -> Int
getCost xs = case dijkstra neighbours calcCost finished xs of
                Nothing -> 10000000
                Just (_,ys) -> length ys

neighbours :: Input -> [Input]
neighbours xs = map (\p -> xs { pos = p }) ps
  where
    (x,y) = pos xs
    ps = filter (validMove xs) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

calcCost :: Input -> Input -> Int
calcCost xs ys = if validMove xs (pos ys)
                 then (h' - h) + 2
                 else 100000
    where
        h  = (elevations xs) M.! (pos xs)
        h' = (elevations xs) M.! (pos ys)

validMove :: Input -> (Int, Int) -> Bool
validMove xs (a,b) = a >= 1 
                  && a <= M.nrows (elevations xs)
                  && b >= 1
                  && b <= M.ncols (elevations xs)
                  && (h' - h) <= 1
    where
        h  = (elevations xs) M.! (pos xs)
        h' = (elevations xs) M.! (a,b)

finished :: Input -> Bool
finished xs = pos xs == endPos xs

-- ################################################################################

part2 :: Input -> Int
part2 xs = minimum ys
    where 
        ys = map (\p -> getCost (xs { pos = p })) allAs
        es = elevations xs
        allAs = [(x,y) | x <- [1..M.nrows es], y <- [1..M.ncols es], es M.! (x,y) == 1]