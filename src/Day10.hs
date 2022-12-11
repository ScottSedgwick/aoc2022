module Day10 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S
import qualified Data.Matrix as M
import ParserUtils ( eol, string)
import Debug.Trace

type Input = [Maybe Int]

datafile :: FilePath
datafile = "data/Day10.txt"

parser :: A.Parser Input
parser = A.many1 $ do
    parseNoop <|> parseNegAddx <|> parsePosAddx

parseNoop :: A.Parser (Maybe Int)
parseNoop = do
    _ <- string "noop"
    _ <- eol
    pure Nothing

parsePosAddx :: A.Parser (Maybe Int)
parsePosAddx = do
    _ <- string "addx "
    x <- A.decimal
    _ <- eol
    pure $ Just x

parseNegAddx :: A.Parser (Maybe Int)
parseNegAddx = do
    _ <- string "addx -"
    x <- A.decimal
    _ <- eol
    pure $ Just (x * (-1))

-- [21,19,18,21,16,18]
part1 :: Input -> Int
part1 xs = trace (show [a,b,c,d,e,f]) $ sum [a * 20, b * 60, c * 100, d * 140, e * 180, f * 220]
        --  + (zs !! 59) * 60
        --  + (zs !! 99) * 100 
        --  + (zs !! 139) * 140 
        --  + (zs !! 179) * 180
        --  + (zs !! 219) * 220 
  where
    ys = 0:0: t1 xs
    zs = t2 1 ys
    a = zs !! 20
    b = zs !! 60
    c = zs !! 100
    d = zs !! 140
    e = zs !! 180
    f = zs !! 220

t1 :: Input -> [Int]
t1 [] = []
t1 (x:xs) = x' <> t1 xs
  where
    x' = case x of
           Nothing  -> [0]
           (Just y) -> [0,y]

t2 :: Int -> [Int] -> [Int]
t2 x [] = []
t2 x (y:ys) = x + y : t2 (x + y) ys

part2 :: Input -> IO ()
part2 xs = do
    let ys = t1 xs
    let zs = t2 1 ys
    print zs
    let es = map calcCoord [1..240]
    let cs = zip es zs
    print cs
    let ds = draw (M.matrix 6 40 (\_ -> False)) cs
    print ds
    prtScr ds

draw :: M.Matrix Bool -> [((Int, Int), Int)] -> M.Matrix Bool
draw m [] = m
draw m (((i,j),y):xs) = draw m' xs
  where
    m' = M.setElem z' (i,j) m
    z' = y >= (j - 1) && y <= (j + 1)


calcCoord :: Int -> (Int, Int)
calcCoord z = (1 + ((z - 1) `div` 40), 1 + ((z - 1) `mod` 40))

prtScr :: M.Matrix Bool -> IO()
prtScr m = mapM_ (prtRow m) [1..6]

prtRow :: M.Matrix Bool -> Int -> IO()
prtRow m r = putStrLn xs
  where
    xs = '#' : init (map (\c -> if M.getElem r c m then '#' else '.') [1..40])
