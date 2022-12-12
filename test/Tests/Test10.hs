module Tests.Test10
  ( module Day10
  , tests
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day10 (Input, datafile, parser, part1, part2)

tests :: Input -> Spec
tests ys = describe "Day 10 tests" $ do
    it "part1" $ do
      part1 ys `shouldBe` 12520
    it "part2" $ do
      let ex = "####.#..#.###..####.###....##..##..#....\n" 
            <> "#....#..#.#..#....#.#..#....#.#..#.#....\n" 
            <> "###..####.#..#...#..#..#....#.#....#....\n" 
            <> "#....#..#.###...#...###.....#.#.##.#....\n" 
            <> "#....#..#.#....#....#....#..#.#..#.#....\n" 
            <> "####.#..#.#....####.#.....##...###.####."
<<<<<<< HEAD
      part2 ys `shouldBe` ex
=======
      part2 ys `shouldBe` ex
>>>>>>> d0d7fa7 (Completed Day 11.)
