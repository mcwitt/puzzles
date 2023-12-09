module Main (main) where

import AoC qualified

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff xs = zipWith (-) (tail xs) xs

diffs = takeWhile (not . all (== 0)) . iterate diff

solve1 = sum . map extrapolate
  where
    extrapolate = foldr ((+) . last) 0 . diffs

solve2 = sum . map extrapolateBackwards
  where
    extrapolateBackwards = foldr ((-) . head) 0 . diffs
