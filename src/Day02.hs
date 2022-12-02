module Day02 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A

data RPS = Rock 
         | Paper 
         | Scissors 
         deriving stock (Show, Eq)

data RPSResult = Win
               | Draw
               | Loss
               deriving stock (Show, Eq)

type Input = [(RPS, RPS)]

datafile :: FilePath
datafile = "data/Day02.txt"

parser :: A.Parser Input
parser = A.many1 $ do
    x <- rpsParser 'A' 'B' 'C'
    _ <- A.char ' '
    y <- rpsParser 'X' 'Y' 'Z'
    _ <- A.endOfLine <|> A.endOfInput
    pure (x,y)

rpsParser :: Char -> Char -> Char -> A.Parser RPS
rpsParser a b c = do
    x <- A.satisfy (\y -> y `elem` [a,b,c])
    if      (x == a) then pure Rock
    else if (x == b) then pure Paper
    else                  pure Scissors

part1 :: Input -> Int
part1 = sum . map score1

score1 :: (RPS, RPS) -> Int
score1 (x, y) = winScore (isWin x y) + shapeScore y

shapeScore :: RPS -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

isWin :: RPS -> RPS -> RPSResult
isWin Rock     Paper    = Win
isWin Scissors Rock     = Win
isWin Paper    Scissors = Win
isWin a        b        = if (a == b) then Draw else Loss

winScore :: RPSResult -> Int
winScore Win  = 6
winScore Draw = 3
winScore Loss = 0

part2 :: Input -> Int
part2 = sum . map score2 . map (\(a,b) -> (a, convert2 b))

convert2 :: RPS -> RPSResult
convert2 Rock     = Loss
convert2 Paper    = Draw
convert2 Scissors = Win

score2 :: (RPS, RPSResult) -> Int
score2 (a, b) = shapeScore (play2 a b) + winScore b

play2 :: RPS -> RPSResult -> RPS
play2 Rock     Win  = Paper
play2 Rock     Loss = Scissors
play2 Paper    Win  = Scissors
play2 Paper    Loss = Rock
play2 Scissors Win  = Rock
play2 Scissors Loss = Paper
play2 a        Draw = a