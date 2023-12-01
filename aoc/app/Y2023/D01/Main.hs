{-# LANGUAGE LambdaCase #-}

module Main where

import qualified AoC
import Data.Char (digitToInt, isDigit)
import Data.List (tails)
import Data.Maybe (mapMaybe)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map getCalibration
  where
    firstDigit = digitToInt . head . dropWhile (not . isDigit)
    lastDigit = firstDigit . reverse
    getCalibration xs = 10 * firstDigit xs + lastDigit xs

parseDigit_ :: String -> Maybe Int
parseDigit_ = \case
  'z' : 'e' : 'r' : 'o' : _ -> Just 0
  'o' : 'n' : 'e' : _ -> Just 1
  't' : 'w' : 'o' : _ -> Just 2
  't' : 'h' : 'r' : 'e' : 'e' : _ -> Just 3
  'f' : 'o' : 'u' : 'r' : _ -> Just 4
  'f' : 'i' : 'v' : 'e' : _ -> Just 5
  's' : 'i' : 'x' : _ -> Just 6
  's' : 'e' : 'v' : 'e' : 'n' : _ -> Just 7
  'e' : 'i' : 'g' : 'h' : 't' : _ -> Just 8
  'n' : 'i' : 'n' : 'e' : _ -> Just 9
  x : _ | isDigit x -> Just $ digitToInt x
  _ -> Nothing

solve2 :: [String] -> Int
solve2 = sum . map getCalibration
  where
    firstDigit = head . mapMaybe parseDigit_ . tails
    lastDigit = head . mapMaybe parseDigit_ . reverse . tails
    getCalibration xs = 10 * firstDigit xs + lastDigit xs
