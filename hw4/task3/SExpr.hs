module Haskell.Hw4.Task3.SExpr where

import Haskell.Hw4.Task3.AParser
import Control.Applicative
import Data.Char

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
  deriving (Show)

data SExpr = A Atom | Comb [SExpr]
  deriving (Show)

parseAtom:: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseExpr:: Parser SExpr
parseExpr = spaces *> sexpr <* spaces
  where
    sexpr = A <$> parseAtom <|> Comb <$> (char '(' *> zeroOrMore parseExpr <* char ')')