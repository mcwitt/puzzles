module Main where

import AoC qualified
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Card = MkCard
  { winningNumbers :: [Int],
    haveNumbers :: [Int]
  }
  deriving (Show)

parse :: String -> [Card]
parse s = head [x | (x, "") <- readP_to_S cards s]
  where
    cards = card `endBy` char '\n'
    card =
      MkCard
        <$ string "Card"
        <* spaces
        <* nat
        <* char ':'
        <* spaces
        <*> (nat `sepBy` spaces)
        <* spaces
        <* char '|'
        <* spaces
        <*> (nat `sepBy` spaces)
    spaces = many1 (satisfy isSpace)
    nat = read <$> many1 (satisfy isDigit)

numMatches (MkCard winningNumbers haveNumbers) =
  Set.size (Set.fromList winningNumbers `Set.intersection` Set.fromList haveNumbers)

solve1 cards = sum [score n | card <- cards, let n = numMatches card]
  where
    score n
      | n >= 1 = 2 ^ (n - 1)
      | otherwise = 0

solve2 = go . map ((1,) . numMatches)
  where
    go [] = 0
    go ((copies, matches) : xs) = copies + go (map (first (+ copies)) (take matches xs) ++ drop matches xs)
