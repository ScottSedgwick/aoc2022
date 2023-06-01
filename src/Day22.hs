module Day22 (Input, datafile, parser, part1, part2) where


import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Maybe (fromJust)
import ParserUtils ( eol, int, restOfLine, string, strN )
import Debug.Trace
import Geometry
import Data.Ix

data Square = Empty | Open | Solid deriving stock (Eq)
instance Show Square where
    show Empty = " "
    show Open  = "."
    show Solid = "#"

data Dirn = L | R deriving stock (Show, Eq)

data Move = Dist Int
          | Turn Dirn
          deriving stock (Eq)
instance Show Move where
    show (Dist x) = show x
    show (Turn d) = show d

data Facing = N | E | W | S deriving stock (Show, Eq)

data Input = Input
  { mp :: M.Matrix Square 
  , moves :: [Move]
  , posn :: (Pt2, Facing)
  } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day22.txt"

-- ################################################################################

parser :: A.Parser Input
parser = do
    xs <- A.many parseMapLine
    let rows = length xs
    let cols = maximum (map length xs)
    let xs' = map (padRight cols Empty) xs
    let m = M.fromList rows cols (concat xs')
    _ <- eol
    ys <- A.many (parseDist <|> parseTurn)
    _ <- eol
    let tl = fromJust $ L.findIndex (==Open) (head xs)
    pure $ Input { mp = m
                 , moves = ys
                 , posn = (Pt2 (tl + 1) 1, E)
                 }

padRight :: Int -> a -> [a] -> [a]
padRight n x xs = xs <> replicate (n - length xs) x

parseMapLine :: A.Parser [Square]
parseMapLine = do
    xs <- A.many (parseEmpty <|> parseOpen <|> parseSolid)
    _ <- eol
    pure xs

parseEmpty :: A.Parser Square
parseEmpty = A.char ' ' >> pure Empty

parseOpen :: A.Parser Square
parseOpen = A.char '.' >> pure Open

parseSolid :: A.Parser Square
parseSolid = A.char '#' >> pure Solid

parseDist :: A.Parser Move
parseDist = do
    x <- int
    pure $ Dist x

parseTurn :: A.Parser Move
parseTurn = parseL <|> parseR
  where
    parseL = A.char 'L' >> pure (Turn L)
    parseR = A.char 'R' >> pure (Turn R)

-- ################################################################################

part1 :: Input -> Int
part1 xs = calcPos (posn zs)
  where
    ms = moves xs
    zs = foldl (\ys mv -> makeMove mv ys) xs ms

calcPos :: (Pt2, Facing) -> Int
calcPos (Pt2 c r, f) = r * 1000 + c * 4 + faceVal f
  where
    faceVal E = 0
    faceVal S = 1
    faceVal W = 2
    faceVal N = 3

makeMove :: Move -> Input -> Input
makeMove (Dist n) xs = 
    case f of
        E -> moveEast n xs
        W -> moveWest n xs
        N -> moveNorth n xs
        S -> moveSouth n xs
    where
        (p, f) = posn xs
makeMove (Turn L) xs = xs { posn = (fst (posn xs), turnL (snd (posn xs)))}
makeMove (Turn R) xs = xs { posn = (fst (posn xs), turnR (snd (posn xs)))}

turnL :: Facing -> Facing
turnL E = N
turnL N = W
turnL W = S
turnL S = E

turnR :: Facing -> Facing
turnR E = S
turnR N = E
turnR W = N
turnR S = W

moveNorth :: Int -> Input -> Input
moveNorth n xs = foldr moveNorth1 xs [1..n] 

moveNorth1 :: Int -> Input -> Input
moveNorth1 _ xs = 
    case mapPt m (Pt2 c r') of
        Nothing -> case mapPt m (Pt2 c i) of
                    Nothing -> undefined
                    Just s' -> case s' of
                                Open  -> xs { posn = (Pt2 c i, f) }
                                Solid -> xs
                                Empty -> undefined
        Just s -> case s of
                    Open  -> xs { posn = (Pt2 c r', f) }  -- next space is open
                    Solid -> xs                           -- next space is blocked
                    Empty -> case mapPt m (Pt2 c i) of
                                Nothing -> undefined
                                Just s' -> case s' of
                                            Open  -> xs { posn = (Pt2 c i, f) }
                                            Solid -> xs
                                            Empty -> undefined 
  where
    m = mp xs
    (Pt2 c r, f) = posn xs 
    r' = r - 1
    cl = M.getCol c m
    i = 1 + V.last (V.findIndices (/= Empty) cl)

moveSouth :: Int -> Input -> Input
moveSouth n xs = foldr moveSouth1 xs [1..n]

moveSouth1 :: Int -> Input -> Input
moveSouth1 _ xs = 
    case mapPt m (Pt2 c r') of
        Nothing -> case mapPt m (Pt2 c i) of
                    Nothing -> undefined
                    Just s' -> case s' of
                                Open  -> xs { posn = (Pt2 c i, f) }
                                Solid -> xs
                                Empty -> undefined
        Just s -> case s of
                    Open  -> xs { posn = (Pt2 c r', f) }  -- next space is open
                    Solid -> xs                           -- next space is blocked
                    Empty -> case mapPt m (Pt2 c i) of
                                Nothing -> undefined
                                Just s' -> case s' of
                                            Open  -> xs { posn = (Pt2 c i, f) }
                                            Solid -> xs
                                            Empty -> undefined 
  where
    m = mp xs
    (Pt2 c r, f) = posn xs 
    r' = r + 1
    cl = M.getCol c m
    i = 1 + fromJust (V.findIndex (/= Empty) cl)

mapPt :: M.Matrix Square -> Pt2 -> Maybe Square
mapPt m (Pt2 r c) = M.safeGet c r m

moveWest :: Int -> Input -> Input
moveWest n xs = foldr moveWest1 xs [1..n] 

moveWest1 :: Int -> Input -> Input
moveWest1 _ xs = 
    case mapPt m (Pt2 c' r) of
        Nothing -> case mapPt m (Pt2 i r) of
                    Nothing -> undefined
                    Just s' -> case s' of
                                Open  -> xs { posn = (Pt2 i r, f) }
                                Solid -> xs
                                Empty -> undefined
        Just s -> case s of  
                    Open  -> xs { posn = (Pt2 c' r, f) }  -- next space is open
                    Solid -> xs                           -- next space is blocked
                    Empty -> case mapPt m (Pt2 i r) of
                                Nothing -> undefined
                                Just s' -> case s' of
                                            Open  -> xs { posn = (Pt2 i r, f) }
                                            Solid -> xs
                                            Empty -> undefined
  where
    m = mp xs
    (Pt2 c r, f) = posn xs 
    c' = c - 1
    rw = M.getRow r m
    i = 1 + V.last (V.findIndices (\x -> x /= Empty) rw)

moveEast :: Int -> Input -> Input
moveEast n xs = foldr moveEast1 xs [1..n]

moveEast1 :: Int -> Input -> Input
moveEast1 _ xs = 
    case mapPt m (Pt2 c' r) of
        Nothing -> case mapPt m (Pt2 i r) of
                    Nothing -> undefined
                    Just s' -> case s' of
                                Open  -> xs { posn = (Pt2 i r, f) }
                                Solid -> xs
                                Empty -> undefined
        Just s -> case s of  
                    Open  -> xs { posn = (Pt2 c' r, f) }  -- next space is open
                    Solid -> xs                           -- next space is blocked
                    Empty -> case mapPt m (Pt2 i r) of
                                Nothing -> undefined
                                Just s' -> case s' of
                                            Open  -> xs { posn = (Pt2 i r, f) }
                                            Solid -> xs
                                            Empty -> undefined
  where
    m = mp xs
    (Pt2 c r, f) = posn xs 
    c' = c + 1
    rw = M.getRow r m
    i = 1 + fromJust (V.findIndex (\x -> x /= Empty) rw)


-- ################################################################################

sides :: [((Int, Int),(Int, Int))]
sides = undefined

part2 :: Input -> Int
part2 xs = 0
