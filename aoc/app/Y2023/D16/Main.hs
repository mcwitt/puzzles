module Main (main) where

import AoC qualified
import Data.Array
import Data.List
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse s =
  let ls = lines s
      h = length ls
      w = length (head ls)
   in listArray ((1, 1), (h, w)) (concat ls)

data Heading = N | E | S | W deriving (Eq, Ord, Show)

type Ray = (Heading, (Int, Int))

solve1 = length . energizedTiles (E, (1, 1))

energizedTiles initRay grid =
  let iters =
        iterate
          (\(rays, seen) -> prune (concatMap propagate rays) seen)
          ([initRay], Set.singleton initRay)
      (_, seen) = head $ dropWhile (\(rays, _) -> not (null rays)) iters
   in nub $ map snd $ Set.toList seen
  where
    prune rays seen =
      ( filter (`Set.notMember` seen) rays,
        foldl' (flip Set.insert) seen rays
      )

    propagate (hdg, pos) =
      let tile = grid ! pos
       in [ (hdg', pos')
            | hdg' <- reflect hdg tile,
              let pos' = step hdg' pos,
              inRange (bounds grid) pos'
          ]

    step N (r, c) = (r - 1, c)
    step E (r, c) = (r, c + 1)
    step S (r, c) = (r + 1, c)
    step W (r, c) = (r, c - 1)

    reflect E '/' = [N]
    reflect E '\\' = [S]
    reflect E '|' = [N, S]
    reflect E '-' = [E]
    reflect N '/' = [E]
    reflect N '\\' = [W]
    reflect N '|' = [N]
    reflect N '-' = [E, W]
    reflect W '/' = [S]
    reflect W '\\' = [N]
    reflect W '|' = [N, S]
    reflect W '-' = [W]
    reflect S '/' = [W]
    reflect S '\\' = [E]
    reflect S '|' = [S]
    reflect S '-' = [E, W]
    reflect dir '.' = [dir]

solve2 grid =
  let ((1, 1), (nr, nc)) = bounds grid
      initRays =
        [(hdg, (r, c)) | r <- [1 .. nr], (c, hdg) <- [(1, E), (nc, W)]]
          ++ [(hdg, (r, c)) | (r, hdg) <- [(1, S), (nr, N)], c <- [1 .. nc]]
   in maximum $ map (length . flip energizedTiles grid) initRays
