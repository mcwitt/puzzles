import AoC (Solution (Solution), mkMain)
import Data.List (foldl', scanl')
import Data.List.Split (splitOn)
import Data.Set qualified as Set
import Linear.V2 (V2 (V2))
import Linear.Vector (zero, (*^))

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

update :: (C4, V2 Int) -> (C4, Int) -> (C4, V2 Int)
update (h, u) (r, d) =
  let h' = h <> r
   in (h', u + d *^ unit h')

manhattan :: (Num a) => V2 a -> a
manhattan (V2 x y) = abs x + abs y

part1 :: [(C4, Int)] -> Int
part1 inp =
  let init = (I, zero)
      (_, p) = foldl' update init inp
   in manhattan p

offsets :: C4 -> [(C4, Int)] -> [V2 Int]
offsets _ [] = []
offsets h ((dh, d) : xs) =
  let h' = h <> dh
   in replicate d (unit h') ++ offsets h' xs

firstRepeat :: (Ord a) => [a] -> Maybe a
firstRepeat = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | Set.member x seen = Just x
      | otherwise = go (Set.insert x seen) xs

part2 :: [(C4, Int)] -> Int
part2 inp =
  let path = scanl' (+) 0 (offsets I inp)
      Just u = firstRepeat path
   in manhattan u
