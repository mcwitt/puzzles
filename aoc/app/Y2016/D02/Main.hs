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

data Three = One | Two | Three
  deriving (Enum)

inc :: Three -> Three
inc One = Two
inc Two = Three
inc Three = Three

dec :: Three -> Three
dec One = One
dec Two = One
dec Three = Two

type Key = (Three, Three)

instance {-# OVERLAPPING #-} Show [Key] where
  show = map (\(r, c) -> let n = 1 + fromEnum c + 3 * fromEnum r in intToDigit n)

upd :: Key -> Dir -> Key
upd (r, c) = go
  where
    go U = (dec r, c)
    go D = (inc r, c)
    go L = (r, dec c)
    go R = (r, inc c)

solve :: (Show [key]) => key -> (key -> Dir -> key) -> [[Dir]] -> String
solve start update = show . tail . scanl' (foldl' update) start

solve1 = solve (Two, Two) upd

data Key2 = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | KA | KB | KC | KD
  deriving (Enum)

instance {-# OVERLAPPING #-} Show [Key2] where
  show = map (\k -> let n = fromEnum k + 1 in if n < 10 then intToDigit n else toEnum (fromEnum 'A' + n - 10))

upd2 :: Key2 -> Dir -> Key2
upd2 K1 D = K3
upd2 K2 D = K6
upd2 K2 R = K3
upd2 K3 U = K1
upd2 K3 D = K7
upd2 K3 L = K2
upd2 K3 R = K4
upd2 K4 D = K8
upd2 K4 L = K3
upd2 K5 R = K6
upd2 K6 U = K2
upd2 K6 D = KA
upd2 K6 L = K5
upd2 K6 R = K7
upd2 K7 U = K3
upd2 K7 D = KB
upd2 K7 L = K6
upd2 K7 R = K8
upd2 K8 U = K4
upd2 K8 D = KC
upd2 K8 L = K7
upd2 K8 R = K9
upd2 K9 L = K8
upd2 KA U = K6
upd2 KA R = KB
upd2 KB U = K7
upd2 KB D = KD
upd2 KB L = KA
upd2 KB R = KC
upd2 KC U = K8
upd2 KC L = KB
upd2 KD U = KB
upd2 k _ = k

solve2 = solve K5 upd2
