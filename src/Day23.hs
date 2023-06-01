module Day23 (Input, datafile, parser, part1, part2) where


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

data Dirn = N | S | W | E deriving stock (Show, Eq)

data Input = Input
  { elves :: S.Set (Int, Int)
  , dirns :: [Dirn]
  } deriving stock (Eq, Show)

datafile :: FilePath
datafile = "data/Day23.txt"

-- ################################################################################

idx :: [Int]
idx = [1..]

parser :: A.Parser Input
parser = do
    xs <- zip idx <$> A.many lineParser
    let ys = concatMap (\(x, ys') -> map (\(y,b) -> ((x,y),b)) ys') xs
    let zs = map fst $ filter snd ys
    pure $ Input { elves = S.fromList zs, dirns = [N,S,W,E] }

lineParser :: A.Parser [(Int, Bool)]
lineParser = do
    xs <- zip idx <$> A.many (emptyP <|> elfP)
    _ <- eol
    pure xs
  where
    emptyP = A.char '.' >> pure False
    elfP = A.char '#' >> pure True

-- ################################################################################

part1 :: Input -> Int
part1 xs = 
    -- trace ("0: " <> show (dirns $ ys !! 0) <> "\n" <> show (draw $ elves $ ys !! 0)) $
    -- trace ("1: " <> show (dirns $ ys !! 1) <> "\n" <> show (draw $ elves $ ys !! 1)) $
    -- trace ("2: " <> show (dirns $ ys !! 2) <> "\n" <> show (draw $ elves $ ys !! 2)) $
    -- trace ("3: " <> show (dirns $ ys !! 3) <> "\n" <> show (draw $ elves $ ys !! 3)) $
    -- trace ("4: " <> show (dirns $ ys !! 4) <> "\n" <> show (draw $ elves $ ys !! 4)) $
    -- trace ("5: " <> show (dirns $ ys !! 5) <> "\n" <> show (draw $ elves $ ys !! 5)) $
    trace (show (elves zs))
    area (elves zs) - (S.size $ elves zs)
  where
    ys = iterate move xs
    zs = ys !! 10

draw :: S.Set (Int, Int) ->  M.Matrix Char
draw xs = 
    M.matrix 12 14 (\(x',y') -> if S.member (x' - 2, y' - 3) xs then '#' else '.')

area :: S.Set (Int, Int) -> Int
area ss = trace (show ((minY, minY), (maxY, maxX))) $ 
    (maxX - minX + 1) * (maxY - minY + 1) 
  where
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys
    xs = map snd $ S.toList ss
    ys = map fst $ S.toList ss

move :: Input -> Input
move (Input es ds) = 
    Input { elves = es', dirns = tail ds <> [head ds]}
  where 
    ns = map (findMove es ds) (S.toList es)
    ms = removeCollisions ns
    es' = foldl updateSet es ms

findMove :: S.Set (Int, Int) -> [Dirn] -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
findMove _  []     _ = Nothing
findMove xs (d:ds) p = if shouldMove xs p && canMove xs d p
                       then Just (p, moveTo p [d])
                       else findMove xs ds p

shouldMove :: S.Set (Int, Int) -> (Int, Int) -> Bool
shouldMove xs p = S.member (moveTo p [N]) xs 
               || S.member (moveTo p [S]) xs 
               || S.member (moveTo p [E]) xs 
               || S.member (moveTo p [W]) xs
               || S.member (moveTo p [N,E]) xs
               || S.member (moveTo p [N,W]) xs
               || S.member (moveTo p [S,E]) xs
               || S.member (moveTo p [S,W]) xs

canMove :: S.Set (Int, Int) -> Dirn -> (Int, Int) -> Bool
canMove xs N p = S.notMember (moveTo p [N,E]) xs && S.notMember (moveTo p [N]) xs && S.notMember (moveTo p [N,W]) xs
canMove xs S p = S.notMember (moveTo p [S,E]) xs && S.notMember (moveTo p [S]) xs && S.notMember (moveTo p [S,W]) xs
canMove xs E p = S.notMember (moveTo p [E,N]) xs && S.notMember (moveTo p [E]) xs && S.notMember (moveTo p [E,S]) xs
canMove xs W p = S.notMember (moveTo p [W,N]) xs && S.notMember (moveTo p [W]) xs && S.notMember (moveTo p [W,S]) xs

moveTo :: (Int, Int) -> [Dirn] -> (Int, Int)
moveTo p ds = foldl moveTo' p ds

moveTo' :: (Int, Int) -> Dirn -> (Int, Int)
moveTo' (y, x) N = (y - 1, x)
moveTo' (y, x) S = (y + 1, x)
moveTo' (y, x) E = (y, x + 1)
moveTo' (y, x) W = (y, x - 1)

removeCollisions :: [Maybe ((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
removeCollisions = concat . filter (\x -> length x == 1) . groupBy (\a b -> snd a == snd b) . sortOn snd . catMaybes

updateSet :: S.Set (Int, Int) -> ((Int, Int), (Int, Int)) -> S.Set (Int, Int)
updateSet xs (p1, p2) = S.insert p2 $ S.delete p1 xs

-- ################################################################################

part2 :: Input -> Int
part2 xs = findFirstSame 1 zs
  where
    ys = map elves $ iterate move xs
    zs = zip ys (tail ys)

findFirstSame :: Int -> [((S.Set (Int, Int), S.Set (Int, Int)))] -> Int
findFirstSame _ [] = (-1)
findFirstSame n ((x,y):xs) = if x == y then n else findFirstSame (n + 1) xs
