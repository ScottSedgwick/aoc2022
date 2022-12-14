{-# LANGUAGE OverloadedStrings #-}
module Day14 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Matrix as M
import Data.List ( findIndex, intercalate, sort )
import ParserUtils 
import Debug.Trace

data ARS = Air | Sand | Rock deriving stock (Eq)
instance Show ARS where
    show Air  = "."
    show Sand = "o"
    show Rock = "#"

data Input = Input
  { minx :: Int
  , maxx :: Int
  , cells :: M.Matrix ARS
  } deriving stock (Show)

datafile :: FilePath
datafile = "data/Day14.txt"

-- ################################################################################

parser :: A.Parser Input
parser = do
    xs <- A.many1 parseLine -- [[(Int, Int)]]
    let y2 = 1 + (maximum $ map snd $ concat xs)
    let x1 = minimum (500 - y2 - 1 : map fst (concat xs))
    let x2 = maximum (500 + y2 + 1 : map fst (concat xs))
    let m = M.matrix y2 (x2 - x1 + 2) (\_ -> Air)
    let m' = foldr (\a b -> insertLine x1 (zip a (tail a)) b) m xs
    pure $ Input { minx = x1, maxx = x2, cells = m' }

insertLine :: Int -> [((Int, Int),(Int, Int))] -> M.Matrix ARS -> M.Matrix ARS
insertLine _ [] m = m
insertLine minx (((x1,y1),(x2,y2)):xs) m | x1 == x2  = insertLine minx xs (insertPoints minx m [(x1,y) | y <- [minimum [y1,y2]..maximum [y1,y2]]])
                                         | y1 == y2  = insertLine minx xs (insertPoints minx m [(x,y1) | x <- [minimum [x1,x2]..maximum [x1,x2]]])
                                         | otherwise = insertLine minx xs m

insertPoints :: Int -> M.Matrix ARS -> [(Int, Int)] -> M.Matrix ARS
insertPoints _ m [] = m
insertPoints minx m ((x,y):xs) = insertPoints minx (insElem minx Rock (x,y) m) xs

insElem :: Int -> ARS -> (Int, Int) -> M.Matrix ARS -> M.Matrix ARS
insElem minx e (x,y) m = M.setElem e (y + 1, x - minx + 1) m

getElem :: Int -> (Int, Int) -> M.Matrix ARS -> ARS
getElem minx (x,y) m = M.getElem (y + 1) (x - minx + 1) m

parseLine :: A.Parser [(Int, Int)]
parseLine = do
    xs <- A.many1 $ do
        x <- A.decimal
        _ <- A.char ','
        y <- A.decimal
        _ <- A.option " " (string " -> ")
        pure (x,y)
    _ <- eol
    pure xs

-- ################################################################################

part1 :: Input -> Int
part1 = length . filter (==Sand) . M.toList . cells . addSand moveSand

addSand :: ((Int, Int) -> Input -> Input) -> Input -> Input
addSand f = f (500,0)

moveSand :: (Int, Int) -> Input -> Input
moveSand (x,y) xs | offScreen (x,y)     xs = xs
                  | clear (x,y + 1)     xs = moveSand (x, y + 1) xs
                  | clear (x - 1,y + 1) xs = moveSand (x - 1, y + 1) xs
                  | clear (x + 1,y + 1) xs = moveSand (x + 1, y + 1) xs
                  | otherwise              = addSand moveSand $ xs { cells = insElem (minx xs) Sand (x,y) (cells xs) }

offScreen :: (Int, Int) -> Input -> Bool
offScreen (x,y) xs = x < minx xs || x > maxx xs || y >= M.nrows (cells xs)

clear :: (Int, Int) -> Input -> Bool
clear p xs = offScreen p xs || getElem (minx xs) p (cells xs) == Air

-- ################################################################################

part2 :: Input -> Int
part2 = length . filter (==Sand) . M.toList . cells . addSand moveSand2 . addFloor

addFloor :: Input -> Input
addFloor xs = xs { cells = M.extendTo Rock (M.nrows m' + 1) (M.ncols m') m' }
  where
    m = cells xs
    m' = M.extendTo Air (M.nrows m + 1) (M.ncols m) m

moveSand2 :: (Int, Int) -> Input -> Input
moveSand2 (x,y) xs | clear (x,y + 1)     xs = moveSand2 (x, y + 1) xs
                   | clear (x - 1,y + 1) xs = moveSand2 (x - 1, y + 1) xs
                   | clear (x + 1,y + 1) xs = moveSand2 (x + 1, y + 1) xs
                   | x == 500 && y == 0     = xs { cells = insElem (minx xs) Sand (x,y) (cells xs) }
                   | otherwise              = addSand moveSand2 $ xs { cells = insElem (minx xs) Sand (x,y) (cells xs) }
