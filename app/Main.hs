module Main (main) where

import Day01

main :: IO ()
main = do
    xs <- rawdata
    let ys = parse xs
    let x = part1 ys
    print x
    let y = part2 ys
    print y
