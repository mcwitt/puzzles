module Main where

import AoC qualified
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

type Mapping = [(Int, Int, Int)]

parse :: String -> ([Int], [Mapping])
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = (,) <$> seeds <* newline <* newline <*> mapping `sepBy` newline
    seeds = string "seeds:" *> spaces *> nat `sepBy` spaces
    newline = char '\n'
    spaces = many1 (satisfy (== ' '))
    nat = read <$> many1 (satisfy isDigit)
    line = many (satisfy (/= '\n')) <* newline
    mapping = line *> mappingElem `endBy` newline
    mappingElem = (,,) <$> nat <* spaces <*> nat <* spaces <*> nat <* optional spaces

apply :: Mapping -> Int -> Int
apply [] x = x
apply ((dst, src, n) : xs) x
  | src <= x && x < src + n = dst + (x - src)
  | otherwise = apply xs x

solve1 :: ([Int], [Mapping]) -> Int
solve1 (seeds, mappings) =
  minimum $ map (\seed -> foldl (flip apply) seed mappings) seeds

type Interval = (Int, Int)

apply' :: Mapping -> Interval -> [Interval]
apply' [] t = [t]
apply' ((dst, src, n) : ms) (a1, b1)
  | b1 < a2 || b2 < a1 = apply' ms (a1, b1) -- no mapping
  | a2 <= a1 && b1 <= b2 = [(a1 + d, b1 + d)] -- complete mapping
  | a1 < a2 && b1 <= b2 = (a2 + d, b1 + d) : apply' ms (a1, a2 - 1) -- right mapped
  | a2 <= a1 && b2 < b1 = (a1 + d, b2 + d) : apply' ms (b2 + 1, b1) -- left mapped
  | a1 < a2 && b2 < b1 = (a2 + d, b2 + d) : concatMap (apply' ms) [(a1, a2 - 1), (b2 + 1, b1)] -- middle mapped
  where
    (a2, b2) = (src, src + n - 1)
    d = dst - src

evens (x : _ : xs) = x : evens xs
evens [x] = [x]
evens _ = []

solve2 :: ([Int], [Mapping]) -> Int
solve2 (xs, mappings) =
  minimum $ map fst $ foldl (\ts m -> concatMap (apply' m) ts) ivals mappings
  where
    ivals = zipWith (\start n -> (start, start + n - 1)) (evens xs) (evens (tail xs))
