module Main (main) where

import AoC qualified
import Data.Bifunctor (first)
import Data.Char (chr, isDigit, isLetter, ord)
import Data.List (group, isInfixOf, sort, sortBy)
import Data.Ord
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Room = MkRoom
  { names :: [String],
    sectorId :: Int,
    checksum :: String
  }
  deriving (Eq, Ord, Show)

name :: Room -> String
name = concat . names

parse :: String -> [Room]
parse = map (unsafeParse room) . lines
  where
    room :: ReadP Room
    room = MkRoom <$> name <* char '-' <*> sector <*> checksum
    name = munch1 isLetter `sepBy` char '-'
    sector = read <$> munch1 isDigit
    checksum = bracketed (munch1 isLetter)
    bracketed p = char '[' *> p <* char ']'

unsafeParse :: ReadP a -> String -> a
unsafeParse p cs = head [a | (a, "") <- readP_to_S p cs]

counts :: (Ord a) => [a] -> [(Int, a)]
counts = map (\xs@(x : _) -> (length xs, x)) . group . sort

computeChecksum :: String -> String
computeChecksum = take 5 . map snd . sortBy (comparing (first Down)) . counts

isValid :: Room -> Bool
isValid room = computeChecksum (name room) == checksum room

solve1 = sum . map sectorId . filter isValid

rotate :: Int -> Char -> Char
rotate n c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')

decryptedName :: Room -> String
decryptedName room = map (rotate (sectorId room)) (name room)

solve2 rooms =
  let [room] = filter (("northpoleobjects" `isInfixOf`) . decryptedName) rooms
   in sectorId room
