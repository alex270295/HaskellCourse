module Haskell.Hw4.Task3.AParser where

import Control.Applicative

import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where 
        (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser rp) = Parser (fmap (first f) . rp)

instance Applicative Parser where
  pure a = Parser (\str -> Just(a, str))

  p1 <*> p2 = Parser f
    where
      f str = case runParser p1 str of
                Nothing -> Nothing
                Just (fRes, strRes) -> first fRes <$> runParser p2 strRes

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f str = (runParser p1 str) <|> (runParser p2 str)

intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)