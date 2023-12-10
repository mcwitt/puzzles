{-# LANGUAGE LambdaCase #-}

module Main (main) where

import AoC qualified
import Data.Foldable (foldl')
import Data.List (sort, union)
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Tile = I | H | NE | NW | SW | SE | Ground | Start deriving (Eq, Show)

type Grid = (Vector Tile, (Int, Int))

type Node = (Int, Int)

parse :: String -> Grid
parse xs = let ls = lines xs in (V.fromList (map mkTile (concat ls)), (length ls, length (head ls)))
  where
    mkTile = \case
      '|' -> I
      '-' -> H
      'L' -> NE
      'J' -> NW
      '7' -> SW
      'F' -> SE
      '.' -> Ground
      'S' -> Start

nodeToIdx (h, w) (r, c)
  | 0 <= r && r < h && 0 <= c && c < w = Just (r * w + c)
  | otherwise = Nothing

idxToNode (h, w) i = i `divMod` w

neighbors :: Grid -> Node -> [Node]
neighbors (v, dims) (r, c) =
  [ n'
    | (n', allowedTiles) <-
        [ ((r, c + 1), [H, NW, SW]),
          ((r, c - 1), [H, SE, NE]),
          ((r + 1, c), [I, NE, NW]),
          ((r - 1, c), [I, SW, SE])
        ],
      i' <- maybeToList (nodeToIdx dims n'),
      let tile' = v V.! i',
      tile' `elem` allowedTiles
  ]

startNode :: Grid -> Node
startNode (v, dims) = idxToNode dims $ fromJust (V.elemIndex Start v)

shortestPaths :: (Ord a) => (a -> [a]) -> [a] -> [[a]]
shortestPaths step = go Set.empty . Seq.fromList . map (: [])
  where
    go _ Empty = []
    go seen (p@(x : _) :<| q)
      | x `Set.member` seen = go seen q
      | otherwise = p : go (Set.insert x seen) (foldl' (|>) q [n : p | n <- step x])

solve1 grid =
  let pathLen = length $ last $ shortestPaths (neighbors grid) [startNode grid]
   in pathLen - 1

solve2 grid@(v, dims@(_, w)) = length [idxToNode dims n | n <- Set.toList otherNodes, isEnclosed n]
  where
    pathIdxs =
      Set.fromList $
        map (fromJust . nodeToIdx dims) $
          foldr1 union $
            take 2 $
              reverse $
                shortestPaths (neighbors grid) [startNode grid]
    allNodes = Set.fromList [0 .. V.length v - 1]
    otherNodes = allNodes `Set.difference` pathIdxs
    isEnclosed n =
      odd $
        crossings
          [ inferTile n'
            | let (r, c) = idxToNode dims n,
              c' <- [c .. w - 1],
              let n' = (r, c'),
              let Just i' = nodeToIdx dims n',
              i' `Set.member` pathIdxs
          ]
    inferTile n@(r, c)
      | label /= Start = label
      | r1 == r2 = H
      | c1 == c2 = I
      | r1 < r2 && c1 < c2 = SW
      | r1 < r2 = SE
      | c1 < c2 = NW
      | otherwise = NE
      where
        Just i = nodeToIdx dims n
        label = v V.! i
        [(r1, c1), (r2, c2)] = neighbors grid n

    crossings xs = length (filter (== I) xs) + go (filter (/= H) xs)
      where
        go (NE : SW : xs) = 1 + go xs
        go (SE : NW : xs) = 1 + go xs
        go (_ : xs) = go xs
        go [] = 0
