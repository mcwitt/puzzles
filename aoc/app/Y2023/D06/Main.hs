module Main where

import AoC qualified
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse :: String -> [(Int, Int)]
parse s = head [x | (x, "") <- readP_to_S races s]
  where
    races = zip <$> times <* newline <*> distances <* newline
    times = string "Time:" *> spaces *> natList
    distances = string "Distance:" *> spaces *> natList
    natList = nat `sepBy` spaces
    spaces = many1 (char ' ')
    nat = read <$> many1 (satisfy isDigit)
    newline = char '\n'

solve1 = product . map numChoices
  where
    numChoices (time, bestDist) =
      length
        [ dist
          | t1 <- [1 .. time],
            let t2 = time - t1,
            let speed = t1,
            let dist = t2 * speed,
            dist > bestDist
        ]

solve2 inp = floor (sqrt (fromIntegral (t ^ 2 - 4 * d)))
  where
    (ts, ds) = unzip inp
    getTimeOrDist xs = read (concatMap show xs)
    t = getTimeOrDist ts
    d = getTimeOrDist ds
