{-# Language RecordWildCards, ViewPatterns #-}
module Day17 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import Control.Lens ( (<&>), (^.), view, Field1(_1), Field2(_2), Field3(_3) )
import Control.Monad ( guard )
import qualified Text.Trifecta as A
import Data.Hashable ( Hashable )
import Data.HashSet        (HashSet)
import Data.HashSet        qualified as S
import Data.List           qualified as L
import GHC.Generics  (Generic)
import Safe ( headMay )

type Input = [Flow]

datafile :: FilePath
datafile = "data/Day17.txt"

-- ################################################################################

parser :: A.Parser Input
parser = A.many $ (parseL <|> parseR)
  where
    parseL = A.char '<' >> pure L
    parseR = A.char '>' >> pure R

-- input

data Flow
  = L
  | R
  deriving stock (Show, Eq)

-- shapes

data Point = Point
  { px :: Int
  , py :: Int
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable Point where

type Shape = [Point]

shapes :: [Int -> Shape]
shapes =
  [ -- h line
    \h ->
    [ Point 2 (h+3), Point 3 (h+3), Point 4 (h+3), Point 5 (h+3) ]
  , -- cross
    \h ->
    [                Point 3 (h+5)
    , Point 2 (h+4), Point 3 (h+4), Point 4 (h+4)
    ,                Point 3 (h+3)
    ]
  , -- L
    \h ->
    [                               Point 4 (h+5)
    ,                               Point 4 (h+4)
    , Point 2 (h+3), Point 3 (h+3), Point 4 (h+3)
    ]
  , -- v line
    \h ->
    [ Point 2 (h+6)
    , Point 2 (h+5)
    , Point 2 (h+4)
    , Point 2 (h+3)
    ]
  , -- square
    \h ->
    [ Point 2 (h+4), Point 3 (h+4)
    , Point 2 (h+3), Point 3 (h+3)
    ]
  ]

moveDown :: Shape -> Shape
moveDown = map (\(Point x y) -> Point x (y-1))

applyFlow :: Flow -> Shape -> Shape
applyFlow f = map (\(Point x y) -> Point (x+d) y)
  where
    d = case f of
          L -> (-1)
          R -> 1


-- world

type World = HashSet Point

fitsIn :: World -> Shape -> Bool
fitsIn w = all (\p@(Point x y) -> x >= 0 && x <= 6 && y >= 0 && not (p `S.member` w))

place :: World -> Shape -> World
place = L.foldl' (flip S.insert)


-- simulation

data RunState = RunState
  { rsWorld   :: World
  , rsFlow    :: [(Int, Flow)]
  , rsNext    :: [(Int, Int -> Shape)]
  , rsHeight  :: Int
  , rsShape   :: Shape
  , rsPlaced  :: Int
  , rsPattern :: [(Shape, Int, Int)]
  }

initialState :: [Flow] -> RunState
initialState f = RunState
  { rsWorld   = mempty
  , rsFlow    = cycle $ zip [0..] f
  , rsNext    = tail $ cycle $ zip [0..] shapes
  , rsHeight  = 0
  , rsShape   = head shapes 0
  , rsPlaced  = 0
  , rsPattern = []
  }

step :: RunState -> RunState
step s@RunState {..} =
  let tryFlow   = applyFlow (snd $ head rsFlow) rsShape
      afterFlow = if fitsIn rsWorld tryFlow then tryFlow else rsShape
      tryDown   = moveDown afterFlow
      newHeight = max rsHeight (maximum (map py rsShape) + 1)
      newWorld  = place rsWorld afterFlow
      relative  = afterFlow <&> \(Point x y) -> Point x (y - rsHeight)
  in if fitsIn rsWorld tryDown
     then s { rsFlow  = tail rsFlow
            , rsShape = tryDown
            }
     else s { rsWorld   = newWorld
            , rsFlow    = tail rsFlow
            , rsNext    = tail rsNext
            , rsHeight  = newHeight
            , rsShape   = snd (head rsNext) newHeight
            , rsPlaced  = rsPlaced + 1
            , rsPattern = (relative, newHeight, fst (head rsFlow)) : rsPattern
            }

findCycle :: [(Shape, Int, Int)] -> Maybe Int
findCycle history = headMay $ do
  let s = length shapes
  size <- [s, 2*s .. div (length history) 2]
  let a = take size history
      b = take size $ drop size history
  guard $ head a ^. _3 == head b ^. _3
  guard $ map (view _1) a == map (view _1) b
  pure size


part1 :: Input -> Int
part1 = rsHeight . until done step . initialState
  where done s = rsPlaced s == 2022

part2 :: Input -> Int
part2 = go 0 . initialState
  where
    go n (step -> s@RunState {..})
      | n == rsPlaced = go n s
      | otherwise = case findCycle rsPattern of
          Nothing          -> go rsPlaced s
          Just patternSize ->
            let (cycles, smallSteps) = divMod (1000000000000 - rsPlaced) patternSize
             in extrapolate patternSize cycles $ apply (rsPlaced + smallSteps) s
    apply n = until (\s -> rsPlaced s == n) step
    extrapolate patternSize cycles RunState {..} =
      let patternHeight = rsHeight - view _2 (rsPattern !! patternSize)
      in rsHeight + cycles * patternHeight

-- 3147
-- part1 :: Input -> Int
-- part1 = rsHeight . until done step . initialState
--   where done s = rsPlaced s == 2022
    
-- ################################################################################

-- part2 :: Input -> Int
-- part2 xs = day17 1_000_000_000_000 xs