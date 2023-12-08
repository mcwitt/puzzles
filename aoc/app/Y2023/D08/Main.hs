module Main (main) where

import AoC qualified
import Data.Char (isAlphaNum)
import Data.List (scanl')
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data LR = L | R deriving (Show)

type Node = String

type Graph = ([Node], Node -> LR -> Node)

mkGraph :: [(Node, (Node, Node))] -> Graph
mkGraph xs =
  let m = Map.fromList xs
   in (map fst xs, \n lr -> proj lr (m Map.! n))
  where
    proj L (x, _) = x
    proj R (_, x) = x

parse :: String -> ([LR], Graph)
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = (,) <$> lrs <* newline <* newline <*> graph
    newline = char '\n'
    lrs = many1 lr
    lr = (L <$ char 'L') <++ (R <$ char 'R')
    graph = mkGraph <$> edgePair `endBy` newline
    nodeName = count 3 (satisfy isAlphaNum)
    edgePair =
      (,)
        <$> nodeName
        <* string " = "
        <*> ((,) <$ char '(' <*> nodeName <* string ", " <*> nodeName <* char ')')

solve1 (lrs, (_, g)) =
  length $
    takeWhile (/= "ZZZ") $
      scanl' g "AAA" $
        cycle lrs

-- |
-- Note: this solution relies on the following unchecked assumptions about the input: Each node sequence
--   * Begins a cycle immediately after the starting node "??A"
--   * Visits only one node labeled "??Z"
--   * Repeats the cycle immediately after "??Z"
--   * Has a cycle length that is a multiple of the LR cycle length
solve2 (lrs, (ns, g)) =
  foldr1
    lcm
    [ cycleLen
      | n <- ns,
        isStart n,
        let cycleLen = length $ takeWhile (not . isEnd) $ scanl' g n $ cycle lrs
    ]
  where
    isStart n = n !! 2 == 'A'
    isEnd n = n !! 2 == 'Z'
