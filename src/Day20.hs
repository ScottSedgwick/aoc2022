module Day20 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import Data.Maybe ( fromJust )
import qualified Data.Sequence as Seq
import ParserUtils ( intLine )

type Input = [Int]

datafile :: FilePath
datafile = "data/Day20.txt"

-- ################################################################################

parser :: A.Parser Input
parser = A.many intLine

-- ################################################################################


part1 :: Input -> Int
part1 xs = grove 1 xs

grove :: Int -> [Int] -> Int
grove n xs = sum [Seq.index s ((z+i)`mod`Seq.length s) | i <- [1000,2000,3000]]
  where
    s = run n xs
    z = fromJust $ Seq.elemIndexL 0 s

run :: Int -> [Int] -> Seq.Seq Int
run n seed = go (Seq.fromList (zip [1..] seed)) (concat (replicate n [1..length seed]))
  where
    go s [] = snd <$> s
    go s (x:xs) = go (Seq.insertAt d (x,v) (b <> a)) xs
        where
            i = fromJust $ Seq.findIndexL (\t -> fst t == x) s
            (a, (_,v) Seq.:<| b) = Seq.splitAt i s
            d = v `mod` (Seq.length s - 1)

-- ################################################################################

part2 :: Input -> Int
part2 xs = grove 10 ys
  where
    ys = map (*811589153) xs