{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import AoC qualified
import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (ap, guard)
import Data.Functor (void)

main = AoC.mkMain solution

solution = AoC.Solution parse solve1 solve2

parse = lines

{- Generic parser combinators -------------------------------------------------}
newtype Parser a = MkParser {runParser :: String -> [(a, String)]}
  deriving (Functor)

runParser_ :: Parser a -> String -> [a]
runParser_ p cs = [a | (a, []) <- runParser p cs]

instance Applicative Parser where
  pure x = MkParser (\cs -> [(x, cs)])
  (<*>) = ap

instance Monad Parser where
  p >>= f =
    MkParser
      ( concatMap
          ( \(a, cs') ->
              let p' = f a in runParser p' cs'
          )
          . runParser p
      )

instance Alternative Parser where
  empty = MkParser (const [])
  p1 <|> p2 = MkParser (\cs -> runParser p1 cs ++ runParser p2 cs)

look = MkParser (\cs -> [(cs, cs)])

satisfy p =
  MkParser
    ( \case
        (c : cs) -> [(c, cs) | p c]
        _ -> []
    )

char c = satisfy (== c)

notFollowedBy p = do
  cs <- look
  case runParser p cs of
    [] -> pure ()
    _ -> empty

{- End of generic parser combinators ------------------------------------------}

solve1 = length . filter supportsTLS

supportsTLS = not . null . runParser_ ipWithTLS

ipWithTLS = do
  part
  (a, b) <- abba
  part
  return (a, b)
  where
    hypernet = inBrackets (many (notFollowedBy abba *> notBracket))
    charOrHypernet = void notBracket <|> void hypernet
    part = many charOrHypernet

abba = do
  a <- notBracket
  b <- notBracket
  guard (a /= b)
  char b
  char a
  return (a, b)

inBrackets p = char '[' *> p <* char ']'

notBracket = satisfy (not . (`elem` "[]"))

solve2 = length . filter supportsSSL

supportsSSL = not . null . runParser_ ipWithSSL

ipWithSSL = abaFirst <|> babFirst
  where
    abaFirst = do
      part
      (a, b) <- aba
      part
      hypernetContext (char b *> char a *> char b)
      part
      return (a, b)

    babFirst = do
      part
      (b, a) <- hypernetContext aba
      part
      char a *> char b *> char a
      part
      return (a, b)

aba = do
  a <- notBracket
  b <- notBracket
  guard (a /= b)
  char a
  return (a, b)

part = many charOrHypernet

charOrHypernet = void notBracket <|> void hypernet

hypernet = inBrackets (many notBracket)

hypernetContext p = inBrackets do
  many notBracket
  a <- p
  many notBracket
  return a
