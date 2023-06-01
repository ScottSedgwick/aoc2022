module Day13 (Input, datafile, parser, part1, part2, printPacket) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import Data.List ( findIndex, intercalate, sort )
import ParserUtils ( eol ) 

type Input = [(Packet, Packet)]

data Packet = Packet [Packet]
            | Value Int
            deriving stock (Show, Eq)

instance Ord Packet where
    compare (Value a)  (Value b)  = compare a b
    compare (Value a)  (Packet b) = compPackets [(Value a)] b
    compare (Packet a) (Value b)  = compPackets a [(Value b)]
    compare (Packet a) (Packet b) = compPackets a b

compPackets :: [Packet] -> [Packet] -> Ordering
compPackets [] [] = EQ
compPackets [] _ = LT
compPackets _ [] = GT
compPackets (x:xs) (y:ys) = case compare x y of
                                LT -> LT
                                GT -> GT
                                EQ -> compPackets xs ys

datafile :: FilePath
datafile = "data/Day13.txt"

printPacket :: Packet -> String
printPacket (Value x) = show x
printPacket (Packet xs) = "[" <> intercalate "," (map printPacket xs) <> "]"

-- ################################################################################

parser :: A.Parser Input
parser = A.many $ do
    xs <- parsePacket
    _ <- eol
    ys <- parsePacket
    _ <- eol
    _ <- eol
    pure (xs, ys)

parsePacket :: A.Parser Packet
parsePacket = do
    _ <- A.char '['
    xs <- A.many $ do
        xs <- parsePacket <|> parseNumber
        _ <- A.option ',' (A.char ',')
        pure xs
    _ <- A.char ']'
    pure $ Packet xs

parseNumber :: A.Parser Packet
parseNumber = (Value . fromIntegral) <$> A.decimal

-- ################################################################################

part1 :: Input -> Int
part1 = sum . map fst . filter (\(_,b) -> b == LT) . zip [1..] . map (\(a,b) -> compare a b)

-- ################################################################################

part2 :: Input -> Int
part2 xs = 
    case i2 of
        Nothing -> (-2)
        (Just x2) -> 
            case i6 of
                Nothing -> (-6)
                (Just x6) -> (x2 + 1) * (x6 + 1)
    where
        index2 = Packet [Packet [Value 2]]
        index6 = Packet [Packet [Value 6]]
        ys = [index2, index6] <> concatMap (\(a,b) -> [a,b]) xs
        zs = sort ys
        i2 = findIndex (== index2) zs
        i6 = findIndex (== index6) zs