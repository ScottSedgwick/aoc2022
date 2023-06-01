{-# LANGUAGE DeriveGeneric #-}
module Day11 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import qualified Data.IntMap.Strict as M
import qualified Data.Text as T
import Data.List (sort, foldl')
import ParserUtils ( eol, string )

data Input = Input
  { ms :: M.IntMap Monkey
  , modulus :: Int
  } deriving stock (Show)

data Monkey = Monkey
  { number :: Int
  , items :: [Int]
  , op :: (Int -> Int)
  , test :: Int
  , onTrue :: Int 
  , onFalse :: Int
  , inspections :: Int
  } 

instance Show Monkey where
    show m = "Monkey " <> show (number m) <> ": " <> show (items m) <> "\n"

datafile :: FilePath
datafile = "data/Day11.txt"

parser :: A.Parser Input
parser = do
    xs <- A.many parseMonkey
    let ys = M.fromList (map (\m -> (number m, m)) xs)
    let z = product (map test xs)
    pure $ Input { ms = ys, modulus = z }

parseMonkey :: A.Parser Monkey
parseMonkey = do
    _ <- string "Monkey " A.<?> "Monkey"
    n <- (fromIntegral <$> A.decimal) A.<?> "Monkey Number"
    _ <- string ":" A.<?> "Monkey Colon"
    _ <- A.restOfLine A.<?> "Monkey EOL"
    _ <- string "  Starting items: " A.<?> "Starting items"
    its <- A.many $ do
        i <- (fromIntegral <$> A.decimal) A.<?> "Items Number"
        _ <- A.option (T.pack " ") (string ", ") A.<?> "Items comma"
        pure i
    _ <- A.restOfLine
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

parseOper :: A.Parser (Either String Int)
parseOper = do
    x <- parseOld <|> (Right <$> (fromIntegral <$> A.decimal))
    _ <- (ignore (string " ")) <|> (ignore A.restOfLine)
    pure x

parseOld :: A.Parser (Either String Int)
parseOld = do
    x <- string "old"
    pure (Left (T.unpack x))

parseOperation :: A.Parser (Int -> Int -> Int)
parseOperation = parseAdd <|> parseMult

parseAdd :: A.Parser (Int -> Int -> Int)
parseAdd = do
    _ <- string "+ "
    pure (+)

parseMult :: A.Parser (Int -> Int -> Int)
parseMult = do
    _ <- string "* "
    pure (*)

parseTest :: A.Parser Int
parseTest = do
    _ <- string "  Test: divisible by "
    x <- fromIntegral <$> A.decimal
    _ <- A.restOfLine
    pure x

parseThrow :: String -> A.Parser Int
parseThrow s = do
    _ <- string ("    If " <> s <> ": throw to monkey ")
    x <- fromIntegral <$> A.decimal
    _ <- eol
    pure x

part1 :: Input -> Int
part1 xs = product $ take 2 zs
  where
    ys = foldl' (\ws _ -> runAllMonkeys ws (\x -> x `div` 3) (M.keys (ms xs))) xs ([1..20] :: [Int])
    zs = reverse $ sort $ map (inspections . snd) (M.toList (ms ys))    

runAllMonkeys :: Input -> (Int -> Int) -> [Int] -> Input
runAllMonkeys xs f ys = foldl' (\b a -> runMonkey a f b) xs ys

runMonkey :: Int -> (Int -> Int) -> Input -> Input
runMonkey n f xs =  xs' { ms = M.adjust (const $ m { items = [], inspections = i }) (number m) (ms xs') }
    where
        m = (ms xs) M.! n
        xs' = foldl' (\b a -> inspect m f b a) xs (items m)
        i = inspections m + length (items m)

inspect :: Monkey -> (Int -> Int) -> Input -> Int -> Input
inspect m f xs y | w `mod` (test m) == 0 = xs { ms = M.adjust (\m' -> m' { items = items m' <> [w] }) (onTrue m)  (ms xs) }
                 | otherwise             = xs { ms = M.adjust (\m' -> m' { items = items m' <> [w] }) (onFalse m) (ms xs) }
    where
        w = (f ((op m) y)) `mod` (modulus xs)

part2 :: Input -> Int
part2 xs = product $ take 2 zs
  where
    ys = foldl' (\ws _ -> runAllMonkeys ws id (M.keys (ms xs))) xs ([1..10000] :: [Int])
    zs = reverse $ sort $ map (inspections . snd) (M.toList (ms ys))
