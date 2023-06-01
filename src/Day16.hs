module Day16 (Input, datafile, parser, part1, part2) where


import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List ( tails )
import ParserUtils ( eol, int, restOfLine, string )

type Input = M.Map String (Int, [String])

datafile :: FilePath
datafile = "data/Day16.txt"

-- ################################################################################

parser :: A.Parser Input
parser = M.fromList <$> (A.many parseCave)
    
parseCave :: A.Parser (String, (Int, [String]))
parseCave = do
    _ <- string "Valve "
    a <- A.anyChar
    b <- A.anyChar
    _ <- string " has flow rate="
    f <- int
    _ <- string "; tunnel leads to valve " <|> string "; tunnels lead to valves "
    xs <- restOfLine
    _ <- eol
    pure ([a,b], (f, splitOn ", " xs))

-- ################################################################################

part1 :: Input -> Int
part1 xs = maximum $ solver xs 30

solver :: M.Map String (Int, [String]) -> Int -> M.Map (S.Set String) Int
solver graph = go [(("AA", S.empty), 0)]
  where
    go states 0 = M.fromListWith max [(open, n) | ((_, open), n) <- states]
    go states t = go (simplify (concatMap step states)) (t-1)
        where
            step ((here, open), n) =
                [((next, open), n) | next <- snd (graph M.! here)] ++
                [((here, S.insert here open), n + (t-1) * amt)
                    | S.notMember here open
                    , let amt = fst (graph M.! here), amt /= 0 ]
    simplify = M.assocs . M.fromListWith max

-- ################################################################################

part2 :: Input -> Int
part2 xs = maximum zs
    where
        ys = solver xs 26
        zs = [v1+v2 | (open1,v1) : elephants <- tails (M.assocs ys),
                            (open2,v2) <- elephants,
                            S.null (S.intersection open1 open2)]