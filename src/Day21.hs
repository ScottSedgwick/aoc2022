module Day21 (Input, datafile, parser, part1, part2) where


import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust )
import ParserUtils ( eol, string, strN )
import Search ( searchOrder, binSearch )

data Monkey = Num Int
            | Add String String
            | Sub String String
            | Mul String String
            | Div String String
            deriving stock (Show, Eq)
type Input = M.Map String Monkey

datafile :: FilePath
datafile = "data/Day21.txt"

-- ################################################################################

parser :: A.Parser Input
parser = do
    xs <- A.many parseMonkey
    pure $ M.fromList xs

parseMonkey :: A.Parser (String, Monkey)
parseMonkey = do
    n <- strN 4
    _ <- string ": "
    m <- parseNum <|> parseAny '+' Add <|> parseAny '-' Sub <|> parseAny '*' Mul <|> parseAny '/' Div
    pure (n, m)

parseNum :: A.Parser Monkey
parseNum = do
    x <- fromIntegral <$> A.decimal
    _ <- eol
    pure $ Num x

parseAny :: Char -> (String -> String -> Monkey) -> A.Parser Monkey
parseAny c f = do
    x <- strN 4
    _ <- A.char ' '
    _ <- A.char c
    _ <- A.char ' '
    y <- strN 4
    _ <- eol
    pure $ f x y

-- ################################################################################

part1 :: Input -> Int
part1 xs = calc (lkup "root" xs) xs

calc :: Monkey -> Input -> Int
calc (Num x) _ = x
calc (Add a b) xs = (calc (lkup a xs) xs) + (calc (lkup b xs) xs)
calc (Sub a b) xs = (calc (lkup a xs) xs) - (calc (lkup b xs) xs)
calc (Mul a b) xs = (calc (lkup a xs) xs) * (calc (lkup b xs) xs)
calc (Div a b) xs = (calc (lkup a xs) xs) `div` (calc (lkup b xs) xs)

lkup :: String -> Input -> Monkey
lkup n xs = x
  where
    x = fromJust $ M.lookup n xs

-- ################################################################################

part2 :: Input -> Int
part2 xs = 
    case res of
        Nothing -> (-10000000)
        Just a -> fromIntegral a
  where
    res = if (x /= x')
            then findVal y mx xs
            else findVal x my xs
    (nx, ny) = leaves $ lkup "root" xs
    mx = lkup nx xs
    my = lkup ny xs
    x = calc mx xs
    y = calc my xs
    x'  = getValFor 0 mx xs

leaves :: Monkey -> (String, String)
leaves (Num _) = ("","")
leaves (Add a b) = (a,b)
leaves (Sub a b) = (a,b)
leaves (Mul a b) = (a,b)
leaves (Div a b) = (a,b)

-- Search for the value of "humn" that makes calc monkey xs = val
findVal :: Int -> Monkey -> Input -> Maybe Integer
findVal val monkey xs = binSearch val f lims so
  where
    lims = (-100000000000000000, 100000000000000000) :: (Integer, Integer)
    so = searchOrder (f . fromIntegral) lims
    f  = test monkey xs

getValFor :: Int -> Monkey -> Input -> Int
getValFor v m xs = calc m xs'
  where
    xs' = M.adjust (\_ -> Num v) "humn" xs

test :: Monkey -> Input -> Int -> Int
test m xs v = calc m xs'
  where
    xs' = M.adjust (\_ -> Num v) "humn" xs
