module Main (main) where

import AoC qualified
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (splitAt)
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Record = KnownOperational | KnownDamaged | Unknown deriving (Eq, Ord, Show)

data Condition = Operational | Damaged deriving (Eq)

type Row = ([Record], [Int])

parse :: String -> [Row]
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = row `endBy` char '\n'
    row = (,) <$> many1 condition <* char ' ' <*> groupSizes
    condition = (KnownOperational <$ char '.') <++ (KnownDamaged <$ char '#') <++ (Unknown <$ char '?')
    groupSizes = nat `sepBy1` char ','
    nat :: ReadP Int = read <$> many1 (satisfy isDigit)

possiblyDamaged :: Record -> Bool
possiblyDamaged KnownOperational = False
possiblyDamaged KnownDamaged = True
possiblyDamaged Unknown = True

validArrangements :: Row -> [[Condition]]
validArrangements = go
  where
    go (KnownOperational : xs, ns) = map (Operational :) (go (xs, ns))
    go (Unknown : xs, ns) = go (KnownOperational : xs, ns) ++ go (KnownDamaged : xs, ns)
    go (KnownDamaged : xs, n : ns) = case splitAt (n - 1) xs of
      (xs, [])
        | length (filter possiblyDamaged xs) == n - 1 ->
            map (replicate n Damaged ++) (go ([], ns))
      (_, []) -> []
      (xs, yys@(y : ys))
        | all possiblyDamaged xs && y /= KnownDamaged ->
            map ((replicate n Damaged ++ [Operational]) ++) (go (ys, ns))
      (_, _ : _) -> []
    go (xs@(KnownDamaged : _), []) = []
    go ([], _ : _) = []
    go ([], []) = [[]]

solve1 rows = sum [length (validArrangements row) | row <- rows]

memoFix f k = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      v <- f (memoFix f) k
      modify (Map.insert k v)
      return v

countValidArrangements :: Row -> Int
countValidArrangements = flip evalState Map.empty . memoFix go
  where
    go f (KnownOperational : xs, ns) = f (xs, ns)
    go f (Unknown : xs, ns) = do
      n1 <- f (KnownOperational : xs, ns)
      n2 <- f (KnownDamaged : xs, ns)
      return (n1 + n2)
    go f (KnownDamaged : xs, n : ns) = case splitAt (n - 1) xs of
      (xs, []) | length (filter possiblyDamaged xs) == n - 1 -> f ([], ns)
      (_, []) -> return 0
      (xs, yys@(y : ys)) | all possiblyDamaged xs && y /= KnownDamaged -> f (ys, ns)
      (_, _ : _) -> return 0
    go _ (xs@(KnownDamaged : _), []) = return 0
    go _ ([], _ : _) = return 0
    go _ ([], []) = return 1

solve2 rows =
  let n = 5
   in sum
        [ countValidArrangements row'
          | row <- rows,
            let row' = foldr1 concatRows (replicate n row)
        ]
  where
    concatRows (xs1, gs1) (xs2, gs2) = (xs1 ++ [Unknown] ++ xs2, gs1 ++ gs2)
