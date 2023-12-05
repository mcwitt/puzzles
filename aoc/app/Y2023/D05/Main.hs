module Main where

import AoC qualified
import Data.Char (isDigit, isSpace)
import Data.Semigroup (Endo (Endo, appEndo))
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

mkEndo :: Mapping -> Endo Int
mkEndo [] = Endo id
mkEndo ((dst, src, n) : xs) =
  Endo
    ( \x ->
        if src <= x && x < src + n
          then dst + (x - src)
          else appEndo (mkEndo xs) x
    )

solve1 (seeds, mappings) =
  minimum $ map (appEndo (foldMap mkEndo (reverse mappings))) seeds

solve2 (flatRanges, mappings) =
  let ranges = chunksOf 2 flatRanges
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
      seeds = ranges >>= \[start, len] -> [start .. start + len - 1]
   in minimum $ map (appEndo (foldMap mkEndo (reverse mappings))) seeds
