module Day07 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Map as M
import Data.List ( sort )
import ParserUtils ( eol, int, restOfLine, string )

data Element = File String Int
             | Dir String
             deriving stock (Show, Eq)

type Input = M.Map String [Element]

datafile :: FilePath
datafile = "data/Day07.txt"

parser :: A.Parser Input
parser = parseCd [] M.empty

parseCd :: [String] -> Input -> A.Parser Input
parseCd ds m = a <|> b
  where
    a = do
        A.eof
        pure m
    b = do
        _ <- string "$ cd "
        x <- restOfLine
        _ <- eol
        if x == ".."
            then parseCd (tail ds) m
            else do
                xs <- parseLs (x <> concat ds)
                parseCd (x:ds) (M.insert (concat ds <> x) xs m)

parseLs :: String -> A.Parser [Element]
parseLs d = do
    _ <- string "$ ls"
    _ <- eol 
    A.many $ (parseDir d <|> parseFile)

parseDir :: String -> A.Parser Element
parseDir d = do
    _ <- string "dir "
    x <- restOfLine
    _ <- eol
    pure $ Dir (d <> x)

parseFile :: A.Parser Element
parseFile = do
    x <- int
    _ <- A.char ' '
    y <- restOfLine
    _ <- eol
    pure $ File y x

part1 :: Input -> Int
part1 xs = sum ws
  where
    zs = map (snd . getSize xs) (M.toList xs)
    ws = filter (<= 100000) zs 

getSize :: Input -> (String, [Element]) -> (String, Int)
getSize xs (d, ys) = (d, sum (map (calcSize xs) ys))

calcSize :: Input -> Element -> Int
calcSize _  (File _ x) = x
calcSize xs (Dir d)    = case M.lookup d xs of
                           Nothing  -> 0
                           (Just ys) -> sum (map (calcSize xs) ys)

part2 :: Input -> Int
part2 xs = head ys
  where
    disk = 70000000
    need = 30000000
    zs = sort $ map (snd . getSize xs) (M.toList xs)
    used = last zs
    unused = disk - used
    minsize = need - unused
    ys = filter (>= minsize) zs 

