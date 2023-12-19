module Main (main) where

import AoC qualified
import Data.Char
import Data.List
import Numeric (readHex)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

data Dir = R | L | U | D deriving (Show)

parse :: String -> [((Dir, Int), String)]
parse s = head [x | (x, "") <- readP_to_S input s]
  where
    input = instruction `endBy` char '\n'
    instruction =
      (,)
        <$> ((,) <$> dir <* char ' ' <*> nat)
        <* char ' '
        <*> parenthesized color
    dir =
      foldr1
        (<++)
        [ R <$ char 'R',
          L <$ char 'L',
          U <$ char 'U',
          D <$ char 'D'
        ]
    nat = read <$> many1 (satisfy isDigit)
    parenthesized p = char '(' *> p <* char ')'
    color = char '#' *> count 6 (satisfy isHexDigit)

move R n (r, c) = (r, c + n)
move L n (r, c) = (r, c - n)
move U n (r, c) = (r - n, c)
move D n (r, c) = (r + n, c)

solve1 instructions =
  let perimeter = sum [d | ((_, d), _) <- instructions]
      vertices = scanl (flip (uncurry move)) (0, 0) $ map fst instructions
   in perimeter `div` 2 + abs (signedArea vertices) + 1

signedArea xs = sum [x1 * y2 - x2 * y1 | ((x1, y1), (x2, y2)) <- zip xs (tail xs)] `div` 2

decodeInstruction xs =
  let [(hexVal, "")] = readHex (take 5 xs)
   in (readDir (last xs), hexVal)
  where
    readDir '0' = R
    readDir '1' = D
    readDir '2' = L
    readDir '3' = U

solve2 instructions =
  let realInstructions = [decodeInstruction hexStr | (_, hexStr) <- instructions]
      perimeter = sum [d | (_, d) <- realInstructions]
      vertices = scanl (flip (uncurry move)) (0, 0) realInstructions
   in perimeter `div` 2 + abs (signedArea vertices) + 1
