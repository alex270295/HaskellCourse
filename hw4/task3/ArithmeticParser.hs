import Haskell.Hw4.Task3.SExpr
import Haskell.Hw4.Task3.AParser
import Control.Applicative
import Data.Char
import Data.List

data Expression = E MultDivide Expression'
data Expression' = Plus MultDivide Expression' | Minus MultDivide Expression' | Expression'
data MultDivide = MD Power MultDivide'
data MultDivide' = Div Power MultDivide' | Mult Power MultDivide' | MultDivide'
data Power = P Terminal Power'
data Power' = Power Terminal Power' | Power'
data Terminal = T Integer | Brackets Expression

simplify :: String -> String
simplify str
    | (isPrefixOf "((" str) && (isSuffixOf "))" str) = tail (init str)
    | otherwise = str

instance Show Expression where
    show s = simplify (show' s)
        where
            show' (E f s) = "(" ++ show f ++ show s ++ ")" 

instance Show Expression' where
    show (Plus f s) = (" + " ++ show f) ++ show s
    show (Minus f s) = (" - " ++ show f) ++ show s
    show e = ""

instance Show MultDivide where
    show s = simplify (show' s)
        where
            show' (MD f s) = "(" ++ show f ++ show s ++ ")" 

instance Show MultDivide' where
    show (Div f s) = (" / " ++ show f) ++ show s
    show (Mult f s) = (" * " ++ show f) ++ show s
    show e = ""

instance Show Power where
    show s = simplify (show' s)
        where
            show' (P f s) = "(" ++ show f ++ show s ++ ")" 

instance Show Power' where
    show (Power f s) = (" ^ " ++ show f) ++ show s
    show e = ""

instance Show Terminal where
    show (T i) = show i
    show (Brackets e) = ('(' : show e) ++ ")"

parseExpression :: Parser Expression
parseExpression = E <$> parseMultDivide <*> parseExpression'

parseExpression' :: Parser Expression'
parseExpression' = Plus <$> (char '+' *> parseMultDivide) <*> parseExpression'
                   <|> Minus <$> (char '-' *> parseMultDivide) <*> parseExpression'
                   <|> pure Expression'

parseMultDivide :: Parser MultDivide
parseMultDivide =  MD <$> parsePower <*> parseMultDivide'

parseMultDivide' :: Parser MultDivide'
parseMultDivide' = Mult <$> (char '*' *> parsePower) <*> parseMultDivide'
             <|> Div <$> (char '/' *> parsePower) <*> parseMultDivide'
             <|> pure MultDivide'

parsePower :: Parser Power
parsePower = P <$> parseTerminal <*> parsePower'

parsePower' :: Parser Power'
parsePower' = Power <$> (char '^' *> parseTerminal) <*> parsePower'
                   <|> pure Power'
parseTerminal :: Parser Terminal
parseTerminal = T <$> posInt
            <|> Brackets <$> (char '(' *> parseExpression <* char ')')

filterNonSpaces :: String -> String
filterNonSpaces str = filter (not . isSpace) str

parse :: [Char] -> Expression
parse str = case runParser parseExpression (filterNonSpaces str) of
            Nothing -> error ("bad input")
            Just (x, "") -> x
            Just (_, y) -> error ("unused input " ++ y)