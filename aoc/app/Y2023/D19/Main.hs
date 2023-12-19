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

proj :: Category -> Part a -> a
proj X (MkPart x _ _ _) = x
proj M (MkPart _ m _ _) = m
proj A (MkPart _ _ a _) = a
proj S (MkPart _ _ _ s) = s

inj :: Category -> a -> Part a -> Part a
inj X x' (MkPart x m a s) = MkPart x' m a s
inj M m' (MkPart x m a s) = MkPart x m' a s
inj A a' (MkPart x m a s) = MkPart x m a' s
inj S s' (MkPart x m a s) = MkPart x m a s'

step :: Part Int -> Workflow -> Label
step part ([], def) = def
step part ((category, comparator, threshold, toLabel) : cases, def)
  | compare (proj category part) threshold == comparator = toLabel
  | otherwise = step part (cases, def)

data Status = Accepted | Rejected deriving (Eq, Show)

untilLeft :: (a -> Either r a) -> a -> r
untilLeft f x = case f x of
  Right x' -> untilLeft f x'
  Left r -> r

stepToEnd :: Part Int -> Label -> (Label -> Workflow) -> Status
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

instance (Num a, Ord a) => Semigroup (Range a) where
  _ <> Empty = Empty
  Empty <> _ = Empty
  lo1 :.. hi1 <> lo2 :.. hi2
    | hi1 < lo2 || hi2 < lo1 = Empty
    | lo1 <= lo2 && lo2 < hi1 = lo2 :.. min hi1 hi2
    | otherwise = lo1 :.. min hi2 hi1

instance (Num a, Ord a) => Monoid (Range a) where
  mempty = 1 :.. 4000

rangeSize Empty = 0
rangeSize (s :.. e) = e - s + 1

instance (Semigroup a) => Semigroup (Part a) where
  MkPart x1 m1 a1 s1 <> MkPart x2 m2 a2 s2 = MkPart (x1 <> x2) (m1 <> m2) (a1 <> a2) (s1 <> s2)

instance (Monoid a) => Monoid (Part a) where
  mempty = MkPart mempty mempty mempty mempty

type Constraints = Part (Range Int)

branches :: Workflow -> [(Constraints, Label)]
branches (cases, def) =
  let constraints = map mkConstraint cases ++ [mempty]
      contexts = mempty : scanl1 (<>) (map mkConstraintC cases)
      labels = map (\(_, _, _, label) -> label) cases ++ [def]
   in zip (zipWith (<>) constraints contexts) labels
  where
    mkConstraint (category, comparator, threshold, _) = inj category (mkRange threshold comparator) mempty
    mkConstraintC (category, comparator, threshold, _) = inj category (mkRangeC threshold comparator) mempty
    mkRange x LT = 1 :.. (x - 1)
    mkRange x GT = (x + 1) :.. 4000
    mkRangeC x LT = x :.. 4000
    mkRangeC x GT = 1 :.. x

mkConstraintTree :: (Label -> Workflow) -> Constraints -> Label -> Tree Constraints
mkConstraintTree lookup = go
  where
    go _ "R" = Node (MkPart Empty Empty Empty Empty) []
    go cs "A" = Node cs []
    go cs label = Node cs [go cs' label' | (cs', label') <- branches (lookup label)]

constraints :: Tree (Part (Range Int)) -> [Constraints]
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
