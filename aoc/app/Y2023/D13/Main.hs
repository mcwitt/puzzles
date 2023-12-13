module Main where

import AoC qualified
import Data.IntMap qualified as IntMap
import Data.List
import Data.List.Split

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = map lines . splitOn "\n\n"

splitAtPivot xs =
  [ (xs, ys)
    | (xs@(_ : _), ys@(_ : _)) <- zip (inits xs) (tails xs),
      and (zipWith (==) (reverse xs) ys)
  ]

solve1 xss =
  sum
    [ case (findPivot xs, findPivot (transpose xs)) of
        (Just row, Nothing) -> 100 * row
        (Nothing, Just col) -> col
      | xs <- xss
    ]
  where
    findPivot xs = case splitAtPivot xs of
      [(xs', _)] -> Just (length xs')
      [] -> Nothing

solve2 xss =
  sum
    [ case (findPivot xs, findPivot (transpose xs)) of
        (Just col, Nothing) -> col
        (Nothing, Just row) -> 100 * row
      | xs <- xss
    ]
  where
    findPivot xs =
      let possiblePivots = concatMap (map (length . fst) . splitAtPivot) xs
          counts = IntMap.toList $ IntMap.fromListWith (+) (map (,1) possiblePivots)
       in fst <$> find ((== length xs - 1) . snd) counts
