import Data.List

factor :: (Integral a) => [a] -> [a]
factor x = sort $ x >>= factorOne

factorOne x = factorOne' [2..] x
  where
    factorOne' s x
        | x < 0 = error "Invalid argument"
        | x <= 1 = []
        | x `mod` (head s) == 0 = (head s) : (factorOne' s (x `div` (head s)))
        | otherwise = factorOne' (tail s) x