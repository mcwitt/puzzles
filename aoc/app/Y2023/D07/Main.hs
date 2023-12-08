{-# LANGUAGE LambdaCase #-}

module Main where

import AoC qualified
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving (Enum, Eq, Ord, Show)

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

data Kind = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Enum, Ord, Show)

kind cards = case sortBy (comparing Down) $ map length $ group $ sort cards of
  5 : _ -> FiveOfAKind
  4 : _ -> FourOfAKind
  3 : 2 : _ -> FullHouse
  3 : _ -> ThreeOfAKind
  2 : 2 : _ -> TwoPair
  2 : _ -> OnePair
  _ -> HighCard

solve1 = sum . zipWith (*) [1 ..] . map snd . sort . map (first (\xs -> (kind xs, xs)))

fromEnum2 :: Card -> Int
fromEnum2 J = -1
fromEnum2 x = fromEnum x

kind2 cards =
  let (withoutJokers, numJokers) = getJokers cards
   in iterate addWild (kind withoutJokers) !! numJokers
  where
    addWild = \case
      HighCard -> OnePair
      OnePair -> ThreeOfAKind
      TwoPair -> FullHouse
      ThreeOfAKind -> FourOfAKind
      FullHouse -> FourOfAKind
      FourOfAKind -> FiveOfAKind
      FiveOfAKind -> FiveOfAKind
    getJokers =
      foldr
        ( \x (ys, n) -> case x of
            J -> (ys, n + 1)
            y -> (y : ys, n)
        )
        ([], 0)

solve2 = sum . zipWith (*) [1 ..] . map snd . sort . map (first (\xs -> (kind2 xs, map fromEnum2 xs)))
