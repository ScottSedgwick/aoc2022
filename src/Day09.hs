module Day09 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S
import ParserUtils ( eol )

data Dirn = DUp | DDown | DRight | DLeft deriving stock (Eq, Ord, Show)
type Input = [Dirn]

datafile :: FilePath
datafile = "data/Day09.txt"

parser :: A.Parser Input
parser = do
    xs <- A.many1 $ do
        x <- parseDirn
        _ <- A.char ' '
        y <- A.decimal
        _ <- eol
        pure (x, y)
    pure $ concatMap (\(d,r) -> replicate r d) xs

parseDirn :: A.Parser Dirn
parseDirn = p 'U' DUp <|> p 'D' DDown <|> p 'R' DRight <|> p 'L' DLeft
  where
    p c d = A.char c >> pure d

part1 :: Input -> Int
part1 = S.size . snd . last . moveChain initChain
  where
    initChain = replicate 2 ((0,0), (S.singleton (0,0)))

moveChain :: [((Int, Int), S.Set (Int, Int))] -> [Dirn] -> [((Int, Int), S.Set (Int, Int))]
moveChain ps [] = ps
moveChain [] _  = []
moveChain (((x, y), p1s):ps) (d:ds) = moveChain ps' ds
  where
    (x', y') = moveDirn (x, y) d
    ps'      = drag ((x', y'), p1s) ps 

moveDirn :: (Int, Int) -> Dirn -> (Int, Int)
moveDirn (x, y) d = case d of
                        DLeft  -> (x - 1, y)
                        DRight -> (x + 1, y)
                        DUp    -> (x    , y + 1)
                        DDown  -> (x    , y - 1)

drag :: ((Int, Int), S.Set (Int, Int)) -> [((Int, Int), S.Set (Int, Int))] -> [((Int, Int), S.Set (Int, Int))]
drag (x, sx) []          = [(x, S.insert x sx)]
drag (x, sx) ((y,sy):ys) = (x, S.insert x sx) : drag ((moveT x y), sy) ys

moveT :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveT (xh, yh) (xt, yt) | xh > (xt + 1) && yh > (yt + 1) = (xt + 1, yt + 1)
                        | xh < (xt - 1) && yh < (yt - 1) = (xt - 1, yt - 1)
                        | xh > (xt + 1) && yh < (yt - 1) = (xt + 1, yt - 1)
                        | xh < (xt - 1) && yh > (yt + 1) = (xt - 1, yt + 1)
                        | xh > (xt + 1)                  = (xt + 1, yh)
                        | xh < (xt - 1)                  = (xt - 1, yh)
                        | yh > (yt + 1)                  = (xh, yt + 1)
                        | yh < (yt - 1)                  = (xh, yt - 1)
                        | otherwise                      = (xt, yt)

part2 :: Input -> Int
part2 = S.size . snd . last . moveChain initChain
  where
    initChain = replicate 10 ((0,0), (S.singleton (0,0)))

