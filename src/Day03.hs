module Day03 (Input, datafile, parser, part1, part2) where

import qualified Data.Attoparsec.Text as A
import Data.Char ( ord )
import ParserUtils ( strLine )

type Input = [String]

datafile :: FilePath
datafile = "data/Day03.txt"

parser :: A.Parser Input
parser = A.many1 strLine

part1 :: Input -> Int
part1 = sum . map (getValue . findDup)

findDup :: String -> Maybe Char
findDup s = f (take l s) (drop l s)
  where
    l = length s `div` 2
    f []     _  = Nothing
    f (x:xs) ys = if x `elem` ys 
                  then Just x 
                  else f xs ys

getValue :: Maybe Char -> Int
getValue Nothing  = 0
getValue (Just x) = if x `elem` ['a'..'z'] 
                    then ord x - ord 'a' + 1
                    else ord x - ord 'A' + 27

part2 :: Input -> Int
part2 = sum . map (getValue . findCommon) . split3

findCommon :: (String, String, String) -> Maybe Char
findCommon ([],     _ , _ ) = Nothing
findCommon ((x:xs), ys, zs) = if x `elem` ys && x `elem` zs 
                              then Just x 
                              else findCommon (xs, ys, zs)

split3 :: Input -> [(String, String, String)]
split3 (a:b:c:xs) = (a,b,c):split3 xs
split3 _ = []