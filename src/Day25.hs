module Day25 (Input, datafile, parser, part1, part2) where


import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Set as S
import Data.List ( groupBy, sortOn )
import Data.Maybe ( catMaybes )
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Maybe (fromJust)
import ParserUtils ( eol, restOfLine, string, strN )
import Debug.Trace
import Data.Ix ()
import ParserUtils

type Input = [Int]

datafile :: FilePath
datafile = "data/Day25.txt"

-- ################################################################################

parser :: A.Parser [Int]
parser = do
    xs <- A.many strLine
    pure $ map (sToD . toNums) xs

toNums :: [Char] -> [Int]
toNums = map toNum

toNum :: Char -> Int
toNum '2' = 2
toNum '1' = 1
toNum '0' = 0
toNum '-' = (-1)
toNum '=' = (-2)
toNum c = error $ "What is this? [" <> [c] <> "]"

sToD :: [Int] -> Int
sToD xs = foldr (\(x,p) b -> x * (5 ^ p) + b) 0 (zip (reverse xs) [0..])

-- ################################################################################

dToS :: Int -> String
dToS 0 = ""
dToS x = trace ("dToS. x = " <> show x <> ". d = " <> show d <> ". m = " <> show m <> ".") $
    case m of
        4 -> dToS (d + 1) <> "-"
        3 -> dToS (d + 1) <> "="
        2 -> dToS d       <> "2"
        1 -> dToS d       <> "1"
        0 -> dToS d       <> "0"
        _ -> error "divMod Error?"
  where
    (d,m) = divMod x 5

part1 :: Input -> String
part1 xs = trace (show res) $
    dToS res 
  where
    res = sum xs

-- ################################################################################

part2 :: Input -> Int
part2 xs = 0
