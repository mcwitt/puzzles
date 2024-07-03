import AoC (Solution (Solution), mkMain)
import Data.List (foldl', scanl')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Linear.V2 (V2 (V2))
import Linear.Vector ((*^))

main :: IO ()
main = mkMain (Solution parse part1 part2)

data C4 = I | L | U | R deriving (Show, Enum)

instance Semigroup C4 where
  x <> y = toEnum ((fromEnum x + fromEnum y) `mod` 4)

parse :: String -> [(C4, Int)]
parse = map (\(x : xs) -> (mkTurns x, read xs)) . splitOn ", "
  where
    mkTurns 'R' = R
    mkTurns 'L' = L
    mkTurns _ = error "invalid input"

unit :: C4 -> V2 Int
unit I = V2 1 0
unit L = V2 0 1
unit R = V2 0 (-1)
unit U = V2 (-1) 0

offsets :: [(C4, Int)] -> [V2 Int]
offsets xs =
  let (t : ts, ds) = unzip xs
      hs = scanl' (<>) t ts
   in zipWith (\h d -> d *^ unit h) hs ds

manhattan :: (Num a) => V2 a -> a
manhattan (V2 x y) = abs x + abs y

part1 = manhattan . foldl' (+) 0 . offsets

offsets' :: [(C4, Int)] -> [V2 Int]
offsets' xs =
  let (t : ts, ds) = unzip xs
      hs = scanl' (<>) t ts
   in concat $ zipWith (\h d -> replicate d (unit h)) hs ds

firstRepeat :: (Ord a) => [a] -> Maybe a
firstRepeat = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | Set.member x seen = Just x
      | otherwise = go (Set.insert x seen) xs

part2 = manhattan . fromJust . firstRepeat . scanl' (+) 0 . offsets'
