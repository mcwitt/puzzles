module Main (main) where

import AoC qualified
import Data.Char (isDigit, isLetter)
import Data.Functor.Base (TreeF (NodeF))
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tree (Tree (..))
import Text.ParserCombinators.ReadP

data Category = X | M | A | S deriving (Eq, Ord, Show)

type Label = String

type Workflow = ([(Category, Ordering, Int, Label)], Label)

data Part a = MkPart a a a a deriving (Functor, Foldable, Show)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse :: String -> (Map Label Workflow, [Part Int])
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    parse_ inp s = head [x | (x, "") <- readP_to_S inp s]
    input = (,) <$> workflows <* count 2 (char '\n') <*> parts <* char '\n'
    workflows = Map.fromList <$> workflow `sepBy` char '\n'
    workflow = (,) <$> label <*> braced cases
    cases = (,) <$> case_ `endBy` char ',' <*> label
    case_ = (,,,) <$> category <*> comparator <*> nat <* char ':' <*> label
    comparator = (LT <$ char '<') <++ (GT <$ char '>')
    label = many1 (satisfy isLetter)
    braced p = char '{' *> p <* char '}'
    parts = part `sepBy` char '\n'
    part = (fromMap . Map.fromList) <$ char '{' <*> assignment `sepBy` char ',' <* char '}'
    assignment = (,) <$> category <* char '=' <*> nat
    nat = read <$> many1 (satisfy isDigit)
    category = foldr1 (<++) [X <$ char 'x', M <$ char 'm', A <$ char 'a', S <$ char 's']
    fromMap m = case traverse (m Map.!?) [X, M, A, S] of
      Just [x, m, a, s] -> MkPart x m a s

proj X (MkPart x _ _ _) = x
proj M (MkPart _ m _ _) = m
proj A (MkPart _ _ a _) = a
proj S (MkPart _ _ _ s) = s

inj X x (MkPart _ m a s) = MkPart x m a s
inj M m (MkPart x _ a s) = MkPart x m a s
inj A a (MkPart x m _ s) = MkPart x m a s
inj S s (MkPart x m a _) = MkPart x m a s

step :: Part Int -> Workflow -> Label
step part ([], def) = def
step part ((category, comparator, score, toLabel) : cases, def)
  | compare (proj category part) score == comparator = toLabel
  | otherwise = step part (cases, def)

data Status = Accepted | Rejected deriving (Eq, Show)

untilLeft f x = case f x of
  Right x' -> untilLeft f x'
  Left r -> r

stepToEnd part start lookup = untilLeft go start
  where
    go :: Label -> Either Status Label
    go "A" = Left Accepted
    go "R" = Left Rejected
    go label = Right $ step part $ lookup label

solve1 (workflows, parts) =
  sum
    [ partScore
      | part@(MkPart x m a s) <- parts,
        stepToEnd part "in" (workflows Map.!) == Accepted,
        let partScore = x + m + a + s
    ]

data Range a = a :.. a | Empty deriving (Show)

instance (Ord a) => Semigroup (Range a) where
  _ <> Empty = Empty
  Empty <> _ = Empty
  lo1 :.. hi1 <> lo2 :.. hi2
    | hi1 < lo2 || hi2 < lo1 = Empty
    | lo1 <= lo2 && lo2 < hi1 = lo2 :.. min hi1 hi2
    | otherwise = lo1 :.. min hi2 hi1

instance (Bounded a, Ord a) => Monoid (Range a) where
  mempty = minBound :.. maxBound

rangeSize Empty = 0
rangeSize (Score s :.. Score e) = e - s + 1

instance (Semigroup a) => Semigroup (Part a) where
  MkPart x1 m1 a1 s1 <> MkPart x2 m2 a2 s2 = MkPart (x1 <> x2) (m1 <> m2) (a1 <> a2) (s1 <> s2)

instance (Monoid a) => Monoid (Part a) where
  mempty = MkPart mempty mempty mempty mempty

newtype Score = Score {getScore :: Int} deriving (Eq, Ord)

instance Bounded Score where
  minBound = Score 1
  maxBound = Score 4_000

type Constraints = Part (Range Score)

mkConstraintTree :: (Label -> Workflow) -> Constraints -> Label -> Tree Constraints
mkConstraintTree lookup = go
  where
    go _ "R" = Node (MkPart Empty Empty Empty Empty) []
    go cs "A" = Node cs []
    go cs label = Node cs [go cs' label' | (cs', label') <- branches (lookup label)]

    branches (cases, def) =
      let constraints = map mkConstraint cases ++ [mempty]
          contexts = mempty : scanl1 (<>) (map mkConstraintC cases)
          labels = map (\(_, _, _, label) -> label) cases ++ [def]
       in zip (zipWith (<>) constraints contexts) labels
      where
        mkConstraint (category, comparator, score, _) = inj category (mkRange score comparator) mempty
        mkConstraintC (category, comparator, score, _) = inj category (mkRangeC score comparator) mempty
        mkRange x LT = minBound :.. Score (x - 1)
        mkRange x GT = Score (x + 1) :.. maxBound
        mkRangeC x LT = Score x :.. maxBound
        mkRangeC x GT = minBound :.. Score x

constraints :: Tree (Part (Range Score)) -> [Constraints]
constraints = cata alg
  where
    alg :: TreeF Constraints [Constraints] -> [Constraints]
    alg (NodeF x []) = [x]
    alg (NodeF x xss) = [x <> x' | xs <- xss, x' <- xs]

solve2 (workflows, _) =
  let tree = mkConstraintTree (workflows Map.!) mempty "in"
   in -- NOTE: constraints are disjoint by construction, hence summing combinations for each constraint suffices
      sum
        [ numCombos
          | path <- constraints tree,
            let numCombos = product (rangeSize <$> path)
        ]
