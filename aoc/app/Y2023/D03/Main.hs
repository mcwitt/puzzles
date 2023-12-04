{-# LANGUAGE LambdaCase #-}

module Main where

import AoC qualified
import Control.Applicative
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Y2023.D03.Parser (Pos (MkPos))
import Y2023.D03.Parser qualified as P

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Elem = Part Char | PartNumber Int deriving (Eq, Show)

parse :: String -> [(Pos, Elem)]
parse = P.parse input
  where
    input = catMaybes <$> many elem
    elem =
      asum
        [ (\pos n -> Just (pos, PartNumber n)) <$> P.pos <*> nat,
          (\pos c -> Just (pos, Part c)) <$> P.pos <*> P.satisfy (\x -> x /= '.' && x /= '\n'),
          Nothing <$ P.anyChar
        ]
    nat = read <$> some (P.satisfy isDigit)

adjacentPositions (MkPos l c) n =
  MkPos l (c - 1)
    : MkPos l (c + length (show n))
    : concat [[MkPos l' c' | c' <- [c - 1 .. c + length (show n)]] | l' <- [l - 1, l + 1]]

solve1 elems =
  sum
    [ n
      | (p, PartNumber n) <- elems,
        any
          ( \p' -> case Map.lookup p' elemsByPos of
              Just (Part _) -> True
              _ -> False
          )
          (adjacentPositions p n)
    ]
  where
    elemsByPos = Map.fromList elems

solve2 elems =
  sum
    $ mapMaybe
      ( \case
          (_, [x, y]) -> Just (x * y)
          _ -> Nothing
      )
    $ Map.toList
    $ Map.fromListWith
      (++)
      [ (p', [n])
        | (p, PartNumber n) <- elems,
          p' <- adjacentPositions p n,
          Part '*' <- maybeToList (Map.lookup p' elemsByPos)
      ]
  where
    elemsByPos = Map.fromList elems
