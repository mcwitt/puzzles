module Main (main) where

import AoC qualified
import Control.Monad.State
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data RecordedCondition = KnownOperational | KnownDamaged | Unknown deriving (Eq, Ord, Show)

data Condition = Operational | Damaged deriving (Eq)

instance Show Condition where
  show Operational = "."
  show Damaged = "#"

type Row = ([RecordedCondition], [Int])

parse :: String -> [Row]
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = row `endBy` char '\n'
    row = (,) <$> many1 condition <* char ' ' <*> groupSizes
    condition = (KnownOperational <$ char '.') <++ (KnownDamaged <$ char '#') <++ (Unknown <$ char '?')
    groupSizes = nat `sepBy1` char ','
    nat :: ReadP Int = read <$> many1 (satisfy isDigit)

data Status = NotInGroup | InGroup Int deriving (Eq, Ord)

validArrangements :: Row -> [[Condition]]
validArrangements = uncurry (go NotInGroup)
  where
    go NotInGroup (KnownOperational : xs) gs = map (Operational :) (go NotInGroup xs gs)
    go NotInGroup (KnownDamaged : xs) (g : gs) = map (Damaged :) (go (InGroup (g - 1)) xs gs)
    go NotInGroup (KnownDamaged : _) [] = []
    go NotInGroup (Unknown : xs) (g : gs) =
      map (Operational :) (go NotInGroup xs (g : gs))
        ++ map (Damaged :) (go (InGroup (g - 1)) xs gs)
    go NotInGroup (Unknown : xs) [] = map (Operational :) (go NotInGroup xs [])
    go NotInGroup [] (_ : _) = []
    go NotInGroup [] [] = [[]]
    go (InGroup 0) (KnownOperational : xs) gs = map (Operational :) (go NotInGroup xs gs)
    go (InGroup g) (KnownOperational : _) _ | g >= 1 = []
    go (InGroup 0) (KnownDamaged : _) _ = []
    go (InGroup g) (KnownDamaged : xs) gs | g >= 1 = map (Damaged :) (go (InGroup (g - 1)) xs gs)
    go (InGroup 0) (Unknown : xs) gs = map (Operational :) (go NotInGroup xs gs)
    go (InGroup g) (Unknown : xs) gs | g >= 1 = map (Damaged :) (go (InGroup (g - 1)) xs gs)
    go (InGroup _) [] (_ : _) = []
    go (InGroup g) [] _ | g > 0 = []
    go (InGroup 0) [] [] = [[]]

solve1 rows = sum [length arr | row <- rows, let arr = validArrangements row]

memoized f k = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      v <- f k
      modify (Map.insert k v)
      return v

countValidArrangementsMemoized :: Row -> Int
countValidArrangementsMemoized (xs, gs) = evalState (run NotInGroup xs gs) Map.empty
  where
    run = (curry . curry) $ memoized $ (uncurry . uncurry) go

    go NotInGroup (KnownOperational : xs) gs = run NotInGroup xs gs
    go NotInGroup (KnownDamaged : xs) (g : gs) = run (InGroup (g - 1)) xs gs
    go NotInGroup (KnownDamaged : _) [] = return 0
    go NotInGroup (Unknown : xs) (g : gs) = do
      n <- run NotInGroup xs (g : gs)
      m <- run (InGroup (g - 1)) xs gs
      return (n + m)
    go NotInGroup (Unknown : xs) [] = run NotInGroup xs []
    go NotInGroup [] (_ : _) = return 0
    go NotInGroup [] [] = return 1
    go (InGroup 0) (KnownOperational : xs) gs = run NotInGroup xs gs
    go (InGroup g) (KnownOperational : _) _ | g >= 1 = return 0
    go (InGroup 0) (KnownDamaged : _) _ = return 0
    go (InGroup g) (KnownDamaged : xs) gs | g >= 1 = run (InGroup (g - 1)) xs gs
    go (InGroup 0) (Unknown : xs) gs = run NotInGroup xs gs
    go (InGroup g) (Unknown : xs) gs | g >= 1 = run (InGroup (g - 1)) xs gs
    go (InGroup g) [] (_ : _) = return 0
    go (InGroup g) [] _ | g > 0 = return 0
    go (InGroup 0) [] [] = return 1

solve2 rows =
  let n = 5
   in sum
        [ countValidArrangementsMemoized row'
          | row <- rows,
            let row' = foldr1 concatRows (replicate n row)
        ]
  where
    concatRows (xs1, gs1) (xs2, gs2) = (xs1 ++ [Unknown] ++ xs2, gs1 ++ gs2)
