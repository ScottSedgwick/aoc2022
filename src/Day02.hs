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
    A.endOfLine <|> A.endOfInput
    pure (x,y)

rpsParser :: Char -> Char -> Char -> A.Parser RPS
rpsParser a b c = do
    x <- A.char a <|> A.char b <|> A.char c
    if (x == a) then pure Rock
    else if (x == b) then pure Paper
    else pure Scissors

part1 :: Input -> Int
part1 = sum . map score1

score1 :: (RPS, RPS) -> Int
score1 (x, y) = winScore (isWin x y) + shapeScore y

shapeScore :: RPS -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

isWin :: RPS -> RPS -> RPSResult
isWin Rock     Paper    = Win
isWin Rock     Scissors = Loss
isWin Rock     Rock     = Draw
isWin Scissors Paper    = Loss
isWin Scissors Scissors = Draw
isWin Scissors Rock     = Win
isWin Paper    Paper    = Draw
isWin Paper    Scissors = Win
isWin Paper    Rock     = Loss

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
score2 (Rock,     Loss) = shapeScore Scissors + winScore(Loss)
score2 (Rock,     Draw) = shapeScore Rock     + winScore(Draw)
score2 (Rock,     Win)  = shapeScore Paper    + winScore(Win)
score2 (Paper,    Loss) = shapeScore Rock     + winScore(Loss)
score2 (Paper,    Draw) = shapeScore Paper    + winScore(Draw)
score2 (Paper,    Win)  = shapeScore Scissors + winScore(Win)
score2 (Scissors, Loss) = shapeScore Paper    + winScore(Loss)
score2 (Scissors, Draw) = shapeScore Scissors + winScore(Draw)
score2 (Scissors, Win)  = shapeScore Rock     + winScore(Win)