{-# LANGUAGE LambdaCase #-}

module Main where

import AoC qualified
import Data.Char (isDigit)
import Data.Monoid
import Text.ParserCombinators.ReadP

main :: IO ()
main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

type Input = [(LR, Distance)]

data LR = L | R deriving (Show)

newtype Distance = Dist {getDistance :: Int} deriving (Show)

parse :: String -> Input
parse = fst . head . readP_to_S input
  where
    lr = (L <$ char 'L') <++ (R <$ char 'R')
    nat = Dist . read <$> munch isDigit
    pair = (,) <$> lr <*> nat
    pairs = pair `sepBy1` string ", "
    input = pairs <* char '\n'

newtype Turn = T Int

turn :: LR -> Turn
turn L = T 3
turn R = T 1

instance Semigroup Turn where
  T x <> T y = T ((x + y) `mod` 4)

instance Monoid Turn where
  mempty = T 0

mconcats :: (Monoid m) => [m] -> [m]
mconcats = scanl (<>) mempty

newtype Heading = H Turn

headings :: [LR] -> [Heading]
headings = map H . mconcats . map turn

data V2 a = V2 !a !a deriving (Show)

instance (Semigroup a) => Semigroup (V2 a) where
  V2 x1 y1 <> V2 x2 y2 = V2 (x1 <> y1) (x2 <> y2)

instance (Monoid a) => Monoid (V2 a) where
  mempty = V2 mempty mempty

newtype Displacement = Disp (V2 (Sum Int)) deriving (Show, Semigroup, Monoid)

displacement :: Distance -> Heading -> Displacement
displacement (Dist x) =
  let d = Sum x
   in \case
        H (T 0) -> Disp (V2 0 d)
        H (T 1) -> Disp (V2 d 0)
        H (T 2) -> Disp (V2 0 (-d))
        H (T 3) -> Disp (V2 (-d) 0)

manhattan :: Displacement -> Distance
manhattan (Disp (V2 (Sum x) (Sum y))) = Dist (abs x + abs y)

solve1 :: Input -> Int
solve1 inp =
  let (turns, dists) = unzip inp
      hdgs = headings turns
      disps = zipWith displacement dists hdgs
      finalDisp = mconcat disps
      answer = getDistance (manhattan finalDisp)
   in answer

solve2 = id
