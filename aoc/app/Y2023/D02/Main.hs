module Main where

import AoC qualified
import Data.Char (isDigit)
import Data.Monoid (Sum (Sum, getSum))
import Data.Semigroup (Max (Max, getMax))
import Linear (V3 (V3))
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Color = Red | Green | Blue deriving (Show)

parse :: String -> [(Int, [[(Int, Color)]])]
parse s = head [x | (x, "") <- readP_to_S games s]
  where
    games = game `endBy` char '\n'
    game =
      (,)
        <$ string "Game "
        <*> nat
        <* string ": "
        <*> (draw `sepBy` string "; ")
    nat = read <$> many1 (satisfy isDigit)
    draw = quantityColor `sepBy` string ", "
    quantityColor = (,) <$> nat <* char ' ' <*> color
    color =
      foldl1
        (<++)
        [ Red <$ string "red",
          Green <$ string "green",
          Blue <$ string "blue"
        ]

type RGB = V3 Int

mkRGB :: [(Int, Color)] -> RGB
mkRGB =
  fmap getSum
    . foldMap
      ( \(n, c) -> case c of
          Red -> V3 (Sum n) mempty mempty
          Green -> V3 mempty (Sum n) mempty
          Blue -> V3 mempty mempty (Sum n)
      )

solve1 games =
  sum [gameId | (gameId, draws) <- games, all (isPossible . mkRGB) draws]
  where
    isPossible obs = and ((<=) <$> obs <*> bag)
    bag = V3 12 13 14

solve2 games =
  sum
    [ power
      | (_, draws) <- games,
        let minBag = getMax <$> foldMap (fmap Max . mkRGB) draws,
        let power = product minBag
    ]
