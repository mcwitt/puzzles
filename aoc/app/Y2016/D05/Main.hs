module Main (main) where

import AoC qualified
import Crypto.Hash.MD5 qualified
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (digitToInt)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = head . lines

hash :: String -> String
hash = unpack . encode . Crypto.Hash.MD5.hash . pack

solve1 doorId =
  take
    8
    [ c
      | n <- [0 ..],
        let hs = hash (doorId ++ show n),
        let (xs, c : _) = splitAt 5 hs,
        all (== '0') xs
    ]

generate :: String -> [(Int, Char)]
generate doorId =
  [ (pos, c)
    | n <- [0 ..],
      let hs = hash (doorId ++ show n),
      let (xs, p : c : _) = splitAt 5 hs,
      all (== '0') xs,
      let pos = digitToInt p,
      pos <= 7
  ]

first :: (a -> Bool) -> [a] -> a
first p = head . dropWhile (not . p)

solve2 :: String -> String
solve2 doorId =
  let ts = generate doorId
      cs = [c | i <- [0 .. 7], let (_, c) = first (\(pos, _) -> pos == i) ts]
   in cs
