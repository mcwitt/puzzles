module Main (main) where

import AoC qualified
import Data.Foldable (toList)
import Data.List (sort, transpose)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Tri a = MkTri a a a deriving (Show, Foldable)

unsafeFromList :: [a] -> Tri a
unsafeFromList [a, b, c] = MkTri a b c

parse :: String -> [Tri Int]
parse = map (unsafeFromList . map read . words) . lines

isValid :: (Num a, Ord a) => Tri a -> Bool
isValid t = let [a, b, c] = sort (toList t) in c < a + b

solve1 = length . filter isValid

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

solve2 = solve1 . map unsafeFromList . chunks 3 . concat . transpose . map toList
