module Day01 (rawdata, parse, part1, part2) where

type Input = [Int]

datafile :: String
datafile = "data/Day01.txt"

rawdata :: IO String
rawdata = readFile datafile

parse :: String-> Input
parse xs = map (\x -> read x :: Int) (lines xs)

part1 :: Input -> Int
part1 (x:y:xs) = (if y > x then 1 else 0) + part1 (y:xs)
part1 _ = 0

part2 :: Input -> Int
part2 (a:b:c:d:xs) = (if s2 > s1 then 1 else 0) + part2 (b:c:d:xs)
  where
    s1 = a + b + c
    s2 = b + c + d
part2 _ = 0