module Day18 (Input, datafile, parser, part1, part2) where


import qualified Text.Trifecta as A
import qualified Data.Set as S
import Geometry ( Pt3(..), Geo(neighbours, bounds) )
import Data.Ix ( Ix(inRange) )
import ParserUtils ( eol, int )

type Input = S.Set Pt3

datafile :: FilePath
datafile = "data/Day18.txt"

-- ################################################################################

parser :: A.Parser Input
parser = do
    xs <- A.many $ do
        x <- int
        _ <- A.char ','
        y <- int
        _ <- A.char ','
        z <- int
        _ <- eol
        pure (Pt3 x y z)
    pure $ S.fromList xs


-- ################################################################################

part1 :: Input -> Int
part1 xs = length $ filter (\x -> S.notMember x xs) ns
    where
        cs = S.toList xs
        ns = concatMap neighbours cs

-- ################################################################################

part2 :: Input -> Int
part2 xs = 
    length $ filter (\x -> S.member x air && S.notMember x xs) ns
  where 
    cs = S.toList xs
    ns = concatMap neighbours cs
    (lo,hi) = bounds cs
    lo' = lo - ptOne
    hi' = hi + ptOne
    ptOne = Pt3 1 1 1
    air = bfs (step (lo', hi') xs) hi'

bfs :: (S.Set Pt3 -> Pt3 -> [Pt3]) -> Pt3 -> S.Set Pt3
bfs f p = bfs' f (S.singleton p) p

bfs' :: (S.Set Pt3 -> Pt3 -> [Pt3]) -> S.Set Pt3 -> Pt3 -> S.Set Pt3
bfs' f a p | ps == []  = a
           | otherwise = foldr (\b c -> bfs' f c b ) a' ps
  where
    ps = f a p
    a' = foldr S.insert a ps

step :: (Pt3,Pt3) -> S.Set Pt3 -> S.Set Pt3 -> Pt3 -> [Pt3]
step box xs air c = filter (\x -> inRange box x && S.notMember x xs && S.notMember x air) ns
  where 
    ns = neighbours c
