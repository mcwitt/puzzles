{-# LANGUAGE LambdaCase #-}

module Y2023.D03.Parser where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Bifunctor (first)

data Pos = MkPos {posLine :: Int, posCol :: Int} deriving (Eq, Ord, Show)

data Input = MkInput String Pos deriving (Show)

newtype Parser a = Parser {runParser :: Input -> [(a, Input)]}

parse :: Parser a -> String -> a
parse p s = case runParser p (MkInput s (MkPos 0 0)) of
  [(x, _)] -> x
  _ -> error "failed to parse"

instance Functor Parser where
  fmap f x = Parser (fmap (first f) . runParser x)

instance Applicative Parser where
  pure x = Parser (\inp -> pure (x, inp))
  pf <*> px =
    Parser
      ( \inp -> do
          (f, inp') <- runParser pf inp
          (x, inp'') <- runParser px inp'
          pure (f x, inp'')
      )

instance Alternative Parser where
  empty = Parser (const [])
  p <|> q =
    Parser
      ( \inp -> case runParser p inp of
          [] -> runParser q inp
          x -> x
      )

anyChar :: Parser Char
anyChar =
  Parser
    ( \case
        (MkInput (x : xs) (MkPos l c)) | x /= '\n' -> pure (x, MkInput xs (MkPos l (c + 1)))
        (MkInput (x : xs) (MkPos l c)) -> pure (x, MkInput xs (MkPos (l + 1) 0))
        (MkInput [] _) -> empty
    )

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  Parser
    ( \inp -> case runParser anyChar inp of
        [(x, inp')] -> [(x, inp') | p x]
        [] -> []
    )

char :: Char -> Parser Char
char x = satisfy (== x)

string :: String -> Parser String
string = traverse char

pos :: Parser Pos
pos = Parser (\inp@(MkInput _ pos) -> [(pos, inp)])
