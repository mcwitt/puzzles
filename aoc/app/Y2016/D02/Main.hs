{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Main (main) where

import AoC qualified
import Data.Char (intToDigit)
import Data.List (foldl', scanl')

data Dir = U | D | L | R deriving (Read, Show)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse :: String -> [[Dir]]
parse = (map . map) (read . (: [])) . lines

data Three = One | Two | Three deriving (Enum)

inc :: Three -> Three
inc One = Two
inc Two = Three
inc Three = Three

dec :: Three -> Three
dec One = One
dec Two = One
dec Three = Two

type Key = (Three, Three)

update :: Key -> Dir -> Key
update (r, c) = go
  where
    go U = (dec r, c)
    go D = (inc r, c)
    go L = (r, dec c)
    go R = (r, inc c)

keyChar :: Key -> Char
keyChar (r, c) = let n = 1 + fromEnum c + 3 * fromEnum r in intToDigit n

solve :: key -> (key -> Dir -> key) -> (key -> Char) -> [[Dir]] -> String
solve start update keyChar = tail . map keyChar . scanl' (foldl' update) start

solve1 = solve (Two, Two) update keyChar

data Key2
  = K1
  | K2
  | K3
  | K4
  | K5
  | K6
  | K7
  | K8
  | K9
  | KA
  | KB
  | KC
  | KD
  deriving (Enum)

update2 :: Key2 -> Dir -> Key2
update2 K1 D = K3
update2 K2 D = K6
update2 K2 R = K3
update2 K3 U = K1
update2 K3 D = K7
update2 K3 L = K2
update2 K3 R = K4
update2 K4 D = K8
update2 K4 L = K3
update2 K5 R = K6
update2 K6 U = K2
update2 K6 D = KA
update2 K6 L = K5
update2 K6 R = K7
update2 K7 U = K3
update2 K7 D = KB
update2 K7 L = K6
update2 K7 R = K8
update2 K8 U = K4
update2 K8 D = KC
update2 K8 L = K7
update2 K8 R = K9
update2 K9 L = K8
update2 KA U = K6
update2 KA R = KB
update2 KB U = K7
update2 KB D = KD
update2 KB L = KA
update2 KB R = KC
update2 KC U = K8
update2 KC L = KB
update2 KD U = KB
update2 k _ = k

keyChar2 :: Key2 -> Char
keyChar2 k =
  let n = fromEnum k + 1
   in if n < 10 then intToDigit n else toEnum (fromEnum 'A' + n - 10)

solve2 = solve K5 update2 keyChar2
