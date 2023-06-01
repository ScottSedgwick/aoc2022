{-# Language StandaloneDeriving, KindSignatures, GADTs, DataKinds, MonadComprehensions, TemplateHaskell, ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
module Day15 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import ParserUtils ( eol, int, string ) 
import Geometry
import Debug.Trace ( trace )

import Data.Kind (Type)

type Input = [Sensor]
data Sensor = Sensor
  { posn :: Pt2
  , beacon :: Pt2
  , distance :: Int
  } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day15.txt"

-- ################################################################################

parser :: A.Parser Input
parser = A.many $ do
    _ <- string "Sensor at x="
    x <- int
    _ <- string ", y="
    y <- int
    let p = Pt2 x y
    _ <- string ": closest beacon is at x="
    bx <- int
    _ <- string ", y="
    by <- int
    let b = Pt2 bx by
    _ <- eol
    pure $ Sensor { posn = p, beacon = b, distance = manhattan p b }

-- manhattan :: (Int, Int) -> (Int, Int) -> Int
-- manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- ################################################################################

data MapPos = MPEmpty | MPSensor | MPBeacon | MPNonBeacon deriving stock (Eq)

instance Show MapPos where
    show MPEmpty = "."
    show MPSensor = "S"
    show MPBeacon = "B"
    show MPNonBeacon = "#"

part1 :: Input -> Int
part1 xs = length $ filter (==True) ys
  where
    yval = 2000000
    ps = [minx .. maxx]
    minx = minimum (map (\x -> col (posn x) - distance x) xs)
    maxx = maximum (map (\x -> col (posn x) + distance x) xs)
    ys = map (\x -> nonBeacon xs yval x) ps

nonBeacon :: Input -> Int -> Int -> Bool
nonBeacon xs y x = close x y xs && not (isSensor x y xs) && not (isBeacon x y xs)

isSensor :: Int -> Int -> Input -> Bool
isSensor _ _ [] = False
isSensor x y (z:zs) = ((Pt2 x y) == (posn z)) || isSensor x y zs

isBeacon :: Int -> Int -> Input -> Bool
isBeacon _ _ [] = False
isBeacon x y (z:zs) = ((Pt2 x y) == (beacon z)) || isBeacon x y zs

close :: Int -> Int -> Input -> Bool
close _ _ [] = False
close x y (z:zs) = d <= distance z || close x y zs
  where
    d = manhattan (Pt2 x y) (posn z)

-- ################################################################################

part2 :: Input -> Int
part2 xs = p2 $ map (\x -> (col (posn x), row (posn x), col (beacon x), row (beacon x))) xs

p2 :: [(Int, Int, Int, Int)] -> Int
p2 input = trace (show rm) $ head xs
  where 
    sq = todiamond (Pt2 2_000_000 2_000_000) 4_000_000
    rm = removeallof (todiamonds input) [sq]
    xs = [ 4_000_000 * x + y | Pt2 y x <- map fromdiamond rm, y >= 0, y <= 4_000_000, x >= 0, x <= 4_000_000]

data Nat = Z | S Nat

data Box :: Nat -> Type where
  Pt  ::  Box 'Z -- ^ A single point
  Dim ::  !Int {- lo bnd -} -> !Int {- hi bnd -} -> Box n {- lo dim -} -> Box ('S n) -- box extended on axis

deriving stock instance Show (Box n)
deriving stock instance Eq (Box n)
deriving stock instance Ord (Box n)

fromdiamond :: Box ('S ('S 'Z)) -> Pt2
fromdiamond (Dim xpy _ (Dim xmy _ Pt)) = Pt2 ((xpy - xmy) `div` 2) ((xpy + xmy) `div` 2) 

todiamond :: Pt2 -> Int -> Box ('S ('S 'Z))
todiamond (Pt2 y x) r = cover (x+y) r (cover (x-y) r Pt)
  where
    cover x' r' = Dim (x' - r') (x' + r' + 1)

removeallof :: [Box n] -> [Box n] -> [Box n] 
removeallof xs ys = foldl remove1 ys xs
  where remove1 acc x = concatMap (subtractBox x) acc

subtractBox :: Box n -> Box n -> [Box n]
subtractBox b1 b2 =
  case intersectBox b1 b2 of
    Nothing -> [b2]
    Just b  -> subtractBox' b b2

subtractBox' :: Box n -> Box n -> [Box n]
subtractBox' Pt Pt = []
subtractBox' (Dim a b xs) (Dim c d ys) =
  [Dim c a ys | c < a] ++
  [Dim b d ys | b < d] ++
  [Dim a b zs | zs <- subtractBox' xs ys]

intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox Pt Pt = Just Pt
intersectBox (Dim a b xs) (Dim c d ys) =
  [Dim x y zs | let x = max a c, let y = min b d, x < y, zs <- intersectBox xs ys]

todiamonds :: [(Int, Int, Int, Int)] -> [Box ('S ('S 'Z))]
todiamonds input =
  [ todiamond (Pt2 y x) r
     | (x,y,nx,ny) <- input
     , let r = manhattan (Pt2 y x) (Pt2 ny nx)
     ]
