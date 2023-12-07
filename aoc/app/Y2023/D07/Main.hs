{-# LANGUAGE LambdaCase #-}

module Main where

import AoC qualified
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (group, sort)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | J
  | Q
  | K
  | A
  deriving (Bounded, Enum, Eq, Ord, Show)

parse :: String -> [([Card], Int)]
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = handAndBid `endBy` newline
    handAndBid = (,) <$> hand <* char ' ' <*> nat
    hand = count 5 card
    card =
      foldl1
        (<++)
        [ Two <$ char '2',
          Three <$ char '3',
          Four <$ char '4',
          Five <$ char '5',
          Six <$ char '6',
          Seven <$ char '7',
          Eight <$ char '8',
          Nine <$ char '9',
          T <$ char 'T',
          J <$ char 'J',
          Q <$ char 'Q',
          K <$ char 'K',
          A <$ char 'A'
        ]
    newline = char '\n'
    nat :: ReadP Int = read <$> many1 (satisfy isDigit)

data Kind
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Enum, Ord, Show)

kind xs = case sort (map length (group (sort xs))) of
  [5] -> FiveOfAKind
  [1, 4] -> FourOfAKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOfAKind
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> OnePair
  [1, 1, 1, 1, 1] -> HighCard

solve1 = sum . zipWith (*) [1 ..] . map snd . sort . map (first (\xs -> (kind xs, xs)))

fromEnum2 :: Card -> Int
fromEnum2 = \case
  J -> -1
  x -> fromEnum x

kind2 xs = maximum (map kind (expand xs))
  where
    expand :: [Card] -> [[Card]]
    expand [] = [[]]
    expand (J : xs) = [x : ys | x <- [minBound .. maxBound], ys <- expand xs]
    expand (x : xs) = [x : ys | ys <- expand xs]

solve2 = sum . zipWith (*) [1 ..] . map snd . sort . map (first (\xs -> (kind2 xs, map fromEnum2 xs)))
