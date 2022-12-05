module Day05 (Input(..), datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.IntMap as M

data Instruction = Instruction
    { numMoved :: Int
    , from :: Int
    , to :: Int
    } deriving stock  (Show, Eq)

data Input = Input 
    { stacks :: M.IntMap String
    , instructions :: [Instruction]
    } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day05.txt"

parser :: A.Parser Input
parser = do 
    xs <- parseStacks
    _ <- parseStackNumbers
    _ <- A.endOfLine
    _ <- A.endOfLine
    ys <- parseInstructions
    pure $ Input { stacks = buildMap (rotStacks xs), instructions = ys }

parseStacks :: A.Parser [[Maybe Char]]
parseStacks = A.many1 $ parseRow

parseStackNumbers :: A.Parser ()
parseStackNumbers = do
    _ <- A.takeTill (\c -> c == '\n')
    pure ()

parseRow :: A.Parser [Maybe Char]
parseRow = do
    xs <- A.many1 parseBox
    _ <- A.endOfLine <|> A.endOfInput
    pure xs

parseBox :: A.Parser (Maybe Char)
parseBox = do
    x <- parseEmptyBox <|> parseFullBox
    _ <- A.option ' ' (A.char ' ')
    pure x

parseEmptyBox :: A.Parser (Maybe Char)
parseEmptyBox = do
    _ <- A.count 3 (A.char ' ')
    pure Nothing

parseFullBox :: A.Parser (Maybe Char)
parseFullBox = do
    _ <- A.char '['
    x <- A.anyChar
    _ <- A.char ']'
    pure (Just x)

parseInstructions :: A.Parser [Instruction]
parseInstructions = A.many1 $ do
    _ <- A.string (T.pack "move ")
    x <- A.decimal
    _ <- A.string (T.pack " from ")
    y <- A.decimal
    _ <- A.string (T.pack " to ")
    z <- A.decimal
    _ <- A.endOfLine <|> A.endOfInput
    pure $ Instruction { numMoved = x, from = y, to = z}

rotStacks :: [[Maybe Char]] -> [[Char]]
rotStacks xs = map (\x -> catMaybes x) $ trimEmpty $ transpose (map (\x -> x ++ repeat Nothing) xs)

buildMap :: [[Char]] -> M.IntMap String
buildMap = f 1
  where
    f _ [] = M.empty
    f n (x:xs) = M.insert n x (f (n + 1) xs)

trimEmpty :: [[Maybe Char]] -> [[Maybe Char]]
trimEmpty [] = []
trimEmpty (x:xs) = if allNothing x then [] else x:(trimEmpty xs)

allNothing :: [Maybe Char] -> Bool
allNothing xs = all (\x -> x == Nothing) xs

part1 :: Input -> String
part1 xs = concat $ map (take 1) $ M.elems ss
    where
        ss = moveStacks (stacks xs) (instructions xs)

moveStacks :: M.IntMap String -> [Instruction] -> M.IntMap String
moveStacks ss []     =  ss
moveStacks ss (x:xs) = moveStacks (doInstr x ss) xs    

doInstr :: Instruction -> M.IntMap String -> M.IntMap String
doInstr (Instruction {numMoved = c, from = f, to = t}) xs = M.adjust (\x -> ms <> x) t $ M.adjust (drop c) f xs
    where 
        ms = reverse $ take c (xs M.! f)

part2 :: Input -> String
part2 xs = concat $ map (take 1) $ M.elems ss
    where
        ss = moveStacks2 (stacks xs) (instructions xs)

moveStacks2 :: M.IntMap String -> [Instruction] -> M.IntMap String
moveStacks2 ss []     = ss
moveStacks2 ss (x:xs) = moveStacks2 (doInstr2 x ss) xs
    

doInstr2 :: Instruction -> M.IntMap String -> M.IntMap String
doInstr2 (Instruction {numMoved = c, from = f, to = t}) xs = M.adjust (\x -> ms <> x) t $ M.adjust (drop c) f xs
    where
        ms = take c (xs M.! f)
