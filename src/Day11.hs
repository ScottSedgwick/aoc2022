{-# LANGUAGE DeriveGeneric #-}
module Day11 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.IntMap.Strict as M
import qualified Data.Text as T
import Data.List (sort, foldl')
import ParserUtils ( eol, string )

type Input = M.IntMap Monkey

data Monkey = Monkey
  { number :: Int
  , items :: [Integer]
  , op :: (Integer -> Integer)
  , test :: Integer
  , onTrue :: Int 
  , onFalse :: Int
  , inspections :: Integer
  } 

instance Show Monkey where
    show m = "Monkey " <> show (number m) <> ": " <> show (items m) <> "\n"

datafile :: FilePath
datafile = "data/Day11.txt"

parser :: A.Parser Input
parser = do
    ms <- A.many1 parseMonkey
    pure $ M.fromList (map (\m -> (number m, m)) ms)

parseMonkey :: A.Parser Monkey
parseMonkey = do
    _ <- string "Monkey " A.<?> "Monkey"
    n <- A.decimal A.<?> "Monkey Number"
    _ <- string ":" A.<?> "Monkey Colon"
    _ <- A.endOfLine A.<?> "Monkey EOL"
    _ <- string "  Starting items: " A.<?> "Starting items"
    its <- A.many' $ do
        i <- A.decimal A.<?> "Items Number"
        _ <- A.option (T.pack " ") (string ", ") A.<?> "Items comma"
        pure i
    _ <- A.endOfLine
    _ <- string "  Operation: new = " A.<?> "Operation new"
    op1 <- parseOper A.<?> "Oper 1"
    pop <- parseOperation A.<?> "Operation"
    op2 <- parseOper A.<?> "Oper 2"
    let operation = 
            case op1 of
                Left _ -> case op2 of
                            Left _  -> (\x -> pop x x)
                            Right y -> (\x -> pop x y)
                Right y -> case op2 of
                            Left _  -> (\x -> pop x y)
                            Right z -> (\_ -> pop y z) 
    t <- parseTest A.<?> "Test"
    tr <- parseThrow "true" A.<?> "True"
    fl <- parseThrow "false" A.<?> "False"
    _ <- eol
    pure $ Monkey 
      { number = n
      , items = its
      , op = operation
      , test = t
      , onTrue = tr
      , onFalse = fl
      , inspections = 0
      }

ignore :: A.Parser a -> A.Parser ()
ignore p = do
    _ <- p
    pure ()

parseOper :: A.Parser (Either String Integer)
parseOper = do
    x <- parseOld <|> (Right <$> A.decimal)
    _ <- (ignore (string " ")) <|> A.endOfLine
    pure x

parseOld :: A.Parser (Either String Integer)
parseOld = do
    x <- string "old"
    pure (Left (T.unpack x))

parseOperation :: A.Parser (Integer -> Integer -> Integer)
parseOperation = parseAdd <|> parseMult

parseAdd :: A.Parser (Integer -> Integer -> Integer)
parseAdd = do
    _ <- string "+ "
    pure (+)

parseMult :: A.Parser (Integer -> Integer -> Integer)
parseMult = do
    _ <- string "* "
    pure (*)

parseTest :: A.Parser Integer
parseTest = do
    _ <- string "  Test: divisible by "
    x <- A.decimal
    _ <- A.endOfLine
    pure x

parseThrow :: String -> A.Parser Int
parseThrow s = do
    _ <- string ("    If " <> s <> ": throw to monkey ")
    x <- A.decimal
    _ <- eol
    pure x

-- ###########################################################################################################################################
-- 10605
part1 :: Input -> Integer
part1 xs = product $ take 2 zs
  where
    ys = foldl' (\ws _ -> runAllMonkeys ws (\x -> x `div` 3) (M.keys xs)) xs ([1..20] :: [Int])
    zs = reverse $ sort $ map (inspections . snd) (M.toList ys)    

runAllMonkeys :: Input -> (Integer -> Integer) -> [Int] -> Input
runAllMonkeys xs f ys = foldl' (\b a -> runMonkey a f b) xs ys

runMonkey :: Int -> (Integer -> Integer) -> Input -> Input
runMonkey n f xs =  M.adjust (const $ m { items = [], inspections = i }) (number m) xs'
    where
        m = xs M.! n
        xs' = foldl' (\b a -> inspect m f b a)  xs (items m)
        i = inspections m + fromIntegral (length (items m))

inspect :: Monkey -> (Integer -> Integer) -> Input -> Integer -> Input
inspect m f xs y | w `mod` (test m) == 0 = M.adjust (\m' -> m' { items = items m' <> [w] }) (onTrue m) xs
                 | otherwise             = M.adjust (\m' -> m' { items = items m' <> [w] }) (onFalse m) xs
    where
        w = y `seq` f ((op m) y)

-- 20 -> 10197
part2 :: Input -> Integer
part2 xs = product $ take 2 zs
  where
    ys = foldl' (\ws _ -> runAllMonkeys ws id (M.keys xs)) xs ([1..500] :: [Int])
    zs = reverse $ sort $ map (inspections . snd) (M.toList ys)