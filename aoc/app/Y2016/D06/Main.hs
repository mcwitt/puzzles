module Main where

import AoC qualified
import Data.List (group, sort, transpose)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = transpose . lines

counts :: (Ord a) => [a] -> [(Int, a)]
counts = map (\xs@(x : _) -> (length xs, x)) . group . sort

solve1 :: [String] -> String
solve1 = map (snd . maximum . counts)

solve2 :: [String] -> String
solve2 = map (snd . minimum . counts)
