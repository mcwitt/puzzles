module AoC where

data Solution input part1 part2 = Solution (String -> input) (input -> part1) (input -> part2)

mkMain :: (Show part1, Show part2) => Solution input part1 part2 -> IO ()
mkMain (Solution parse solve1 solve2) = interact (\s -> let inp = parse s in show (solve1 inp, solve2 inp))
