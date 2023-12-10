module Main (main) where

import AoC qualified
import Data.List (findIndices, transpose)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = lines

expand = transpose . expandRows . transpose . expandRows
  where
    expandRows = concatMap (\xs -> if all (== '.') xs then [xs, xs] else [xs])

solve1 u =
  sum
    [ dist
      | g1@(r1, c1) <- galaxySectors,
        g2@(r2, c2) <- galaxySectors,
        g1 < g2,
        let dist = abs (r1 - r2) + abs (c1 - c2)
    ]
  where
    u' = expand u
    galaxySectors = [(row, col) | (row, xs) <- zip [0 ..] u', (col, x) <- zip [0 ..] xs, x == '#']

solve2 u =
  sum
    [ dist
      | g1@(r1, c1) <- galaxySectors,
        g2@(r2, c2) <- galaxySectors,
        g1 < g2,
        let dist = abs (expandedDist emptyRowIdxs r1 r2) + abs (expandedDist emptyColIdxs c1 c2)
    ]
  where
    galaxySectors = [(row, col) | (row, xs) <- zip [0 ..] u, (col, x) <- zip [0 ..] xs, x == '#']
    emptyIdxs = findIndices (all (== '.'))
    emptyRowIdxs = emptyIdxs u
    emptyColIdxs = emptyIdxs $ transpose u
    expandedDist emptyIdxs x1 x2 =
      let xmin = min x1 x2
          xmax = max x1 x2
       in sum
            [ dx
              | x <- [xmin .. xmax - 1],
                let dx = if x `elem` emptyIdxs then 1_000_000 else 1
            ]
