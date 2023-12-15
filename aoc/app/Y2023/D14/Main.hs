module Main (main) where

import AoC qualified
import Data.List (foldl', intercalate, sort, sortOn, transpose)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Ord

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = lines

settle = intercalate "#" . map (sortOn Down) . splitOn "#"

solve1 = sum . concatMap (load . settle) . transpose

load xs = [r | ('O', r) <- zip xs (reverse [1 .. length xs])]

settleDown = intercalate "#" . map sort . splitOn "#"

findCycle :: (Ord a) => [a] -> Maybe (Int, Int)
findCycle = go Map.empty . zip [0 ..]
  where
    go _ [] = Nothing
    go seen ((i, x) : xs) = case Map.lookup x seen of
      Just start -> Just (start, i - start)
      Nothing -> go (Map.insert x i seen) xs

solve2 xss =
  let spinCycle xss =
        foldl'
          (\xss' settle -> map settle (transpose xss'))
          xss
          [settle, settle, settleDown, settleDown]
      spinCycles = iterate spinCycle xss
      Just (cycleStart, cycleLength) = findCycle spinCycles
      nSteps = cycleStart + (1_000_000_000 - cycleStart) `mod` cycleLength
      xss' = spinCycles !! nSteps
   in sum (concatMap load (transpose xss'))
