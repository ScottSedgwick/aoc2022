module Day05 (Input(..), datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.IntMap as M

data Instruction = Instruction
    { numMoved :: Integer
    , from :: Integer
    , to :: Integer
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
    _ <- A.restOfLine --parseStackNumbers
    _ <- A.restOfLine
    _ <- A.restOfLine
    ys <- parseInstructions
    pure $ Input { stacks = buildMap (rotStacks xs), instructions = ys }

parseStacks :: A.Parser [[Maybe Char]]
parseStacks = A.many $ parseRow

-- parseStackNumbers :: A.Parser ()
-- parseStackNumbers = do
--     _ <- A.manyTill (\c -> c == '\n')
--     pure ()

parseRow :: A.Parser [Maybe Char]
parseRow = do
    xs <- A.many parseBox
    _ <- A.restOfLine
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
parseInstructions = A.many $ do
    _ <- A.string "move "
    x <- A.decimal
    _ <- A.string " from "
    y <- A.decimal
    _ <- A.string " to "
    z <- A.decimal
    _ <- A.restOfLine
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
doInstr (Instruction {numMoved = c, from = f, to = t}) xs = M.adjust (\x -> ms <> x) (fromIntegral t) $ M.adjust (drop (fromIntegral c)) (fromIntegral f) xs
    where 
        ms = reverse $ take (fromIntegral c) (xs M.! (fromIntegral f))

part2 :: Input -> String
part2 xs = concat $ map (take 1) $ M.elems ss
    where
        ss = moveStacks2 (stacks xs) (instructions xs)

moveStacks2 :: M.IntMap String -> [Instruction] -> M.IntMap String
moveStacks2 ss []     = ss
moveStacks2 ss (x:xs) = moveStacks2 (doInstr2 x ss) xs
    

doInstr2 :: Instruction -> M.IntMap String -> M.IntMap String
doInstr2 (Instruction {numMoved = c, from = f, to = t}) xs = M.adjust (\x -> ms <> x) (fromIntegral t) $ M.adjust (drop (fromIntegral c)) (fromIntegral f) xs
    where
        ms = take (fromIntegral c) (xs M.! (fromIntegral f))
