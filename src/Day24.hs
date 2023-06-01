module Day24 (Input, datafile, parser, part1, part2) where

import Algorithm.Search
import Geometry
import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Set as S
import Data.List ( groupBy, sortOn )
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Maybe (fromJust)
import ParserUtils 
import Debug.Trace
import Data.Ix 
import Data.Map.Strict qualified as SMap


datafile :: FilePath
datafile = "data/Day24.txt"

-- ################################################################################

type Input = M.Map Pt2 Char

parser :: A.Parser Input
parser = do
  xs <- A.some strLine
  let ys = SMap.fromList (coordLines xs)
  pure $ M.filter ('.' /=) ys

coordLines :: [String] -> [(Pt2, Char)]
coordLines rows = [(Pt2 y x, z) | (y,row) <- zip [0..] rows, (x,z) <- zip [0..] row]

-- ################################################################################

part1 :: Input -> Int
part1 xs = t1
  where
    t1 = loop 0 (S.singleton (Pt2 0 1)) target
    loop t best land
      | S.member land best = t
      | otherwise =
        loop (t+1) (grow corner world best) land
          where world = worldAt corner (t+1) xs
    corner = maximum (M.keys xs)
    target = corner + west

grow :: Pt2 -> S.Set Pt2 -> S.Set Pt2 -> S.Set Pt2
grow corner world best =
  S.fromList
    [ next
      | here <- S.toList best
      , next@(Pt2 y x) <- here : neighbours here
      , inRange (0,corner) next
      , S.notMember next world
    ]

worldAt :: Pt2 -> Int -> M.Map Pt2 Char -> S.Set Pt2
worldAt corner t input =
  S.fromList [ location corner k t v | (k,v) <- M.toList input]

location :: Pt2 -> Pt2 -> Int -> Char -> Pt2
location _ here _ '#' = here
location corner here t c
  | Just vec <- charToVec c =
    zipCoord mod (here - 1 + scaleCoord t vec) (corner - 1) + 1
location _ _ _ _ = undefined

scaleCoord :: Int -> Pt2 -> Pt2
scaleCoord n = mapCoord (n *)

mapCoord :: (Int -> Int) -> Pt2 -> Pt2
mapCoord f (Pt2 y x) = Pt2 (f y) (f x)

zipCoord :: (Int -> Int -> Int) -> Pt2 -> Pt2 -> Pt2
zipCoord f (Pt2 y1 x1) (Pt2 y2 x2) = Pt2 (f y1 y2) (f x1 x2)

charToVec :: Char -> Maybe Pt2
charToVec '^' = Just north
charToVec 'v' = Just south
charToVec '>' = Just east
charToVec '<' = Just west
charToVec  _  = Nothing

north :: Pt2
north = Pt2 (-1) 0

-- | Unit vector pointing right
east :: Pt2
east = Pt2 0 1

-- | Unit vector pointing down
south :: Pt2
south = Pt2 1 0

-- | Unit vector pointing left
west :: Pt2
west = Pt2 0 (-1)


-- ################################################################################

part2 :: Input -> Int
part2 xs = trace ("T1: " <> show t1 <> ", T2: " <> show t2 <> ", T3: " <> show t3)
    t3
  where
    t1 = loop 0 (S.singleton (Pt2 0 1)) target
    t2 = loop t1 (S.singleton target) (Pt2 0 1)
    t3 = loop t2 (S.singleton (Pt2 0 1)) target
    loop t best land
      | S.member land best = t
      | otherwise =
        loop (t+1) (grow corner world best) land
          where world = worldAt corner (t+1) xs
    corner = maximum (M.keys xs)
    target = corner + west

-- main :: IO ()
-- main =
--  do input <- M.filter ('.' /=) <$> getInputMap 2022 24

--     let corner = maximum (M.keys input)
--     let target = corner + west

--     let loop t best land
--           | S.member land best = t
--           | otherwise =
--             loop (t+1) (grow corner world best) land
--               where world = worldAt corner (t+1) input

--     let t1 = loop 0 (S.singleton (Pt2 0 1)) target
--         t2 = loop t1 (S.singleton target) (Pt2 0 1)
--         t3 = loop t2 (S.singleton (Pt2 0 1)) target
--     print t1
--     print t3

-- -- | Find the set of obstructions in the world at a given time step
-- worldAt :: Pt2 -> Int -> M.Map Pt2 Char -> S.Set Pt2
-- worldAt corner t input =
--   S.fromList [ location corner k t v | (k,v) <- M.toList input]

-- -- | Given a set of locations the elf could be find the set the elf can be at next.
-- grow :: Pt2 -> S.Set Pt2 -> S.Set Pt2 -> S.Set Pt2
-- grow corner world best =
--   S.fromList
--     [ next
--       | here <- S.toList best
--       , next@(Pt2 y x) <- here : neighbours here
--       , inRange (0,corner) next
--       , S.notMember next world
--     ]

-- -- | Compute the location an obstacle will be at at a given time step
-- location :: Pt2 -> Pt2 -> Int -> Char -> Pt2
-- location _ here _ '#' = here
-- location corner here t c
--   | Just vec <- charToVec c =
--     zipCoord mod (here - 1 + scaleCoord t vec) (corner - 1) + 1
-- location _ _ _ _ = undefined