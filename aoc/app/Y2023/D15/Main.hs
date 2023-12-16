module Main (main) where

import AoC qualified
import Data.Char
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (foldl')
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = splitOn "," . head . lines

hash = foldl' (\z c -> ((z + ord c) * 17) `mod` 256) 0

solve1 = sum . map hash

type Label = String

type FocalLength = Int

data Instruction = Remove Label | Replace Lens

data Lens = MkLens {getLabel :: String, getFocalLength :: Int}

type Boxes = IntMap [Lens]

solve2 = focusingPower . foldl' exec IntMap.empty . map parse
  where
    parse xs = head [x | (x, "") <- readP_to_S instruction xs]
      where
        instruction = remove <++ replace
        remove = Remove <$> label <* char '-'
        replace = Replace <$> (MkLens <$> label <* char '=' <*> nat)
        label = many1 (satisfy isLetter)
        nat = read <$> many1 (satisfy isDigit)

    exec boxes (Remove label) = IntMap.adjust (filter ((/= label) . getLabel)) (hash label) boxes
    exec boxes (Replace newLens@(MkLens label focalLength)) =
      IntMap.insertWith
        ( \_ lenses ->
            case span ((/= label) . getLabel) lenses of
              (lenses, []) -> newLens : lenses
              (xs, _ : ys) -> xs ++ newLens : ys
        )
        (hash label)
        [newLens]
        boxes

    focusingPower boxes =
      sum
        [ power
          | (i, lenses) <- IntMap.toList boxes,
            let power = sum [(i + 1) * j * focalLength | (j, MkLens _ focalLength) <- zip [1 ..] (reverse lenses)]
        ]
