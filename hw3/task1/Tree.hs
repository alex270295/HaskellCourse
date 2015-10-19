{-# LANGUAGE FlexibleInstances #-}
import Data.Maybe
import Data.Monoid

class Set t where
    emptySet :: (Ord a) => t a
    toList :: (Ord a) => t a -> [a]
    find :: (Ord a) => t a -> a -> Bool
    insert :: (Ord a) => t a -> a -> t a
    delete :: (Ord a) => t a -> a -> t a
    next :: (Ord a) => t a -> a -> Maybe a
    prev :: (Ord a) => t a -> a -> Maybe a
    fromList :: (Ord a) => [a] -> t a

type Parent = Tree
type LeftTree = Tree
type RightTree = Tree
data Tree a = Leaf | Node a (LeftTree a) (RightTree a) (Parent a) deriving (Show)

at :: Tree a -> a
at (Node v _ _ _) = v

left :: Tree a -> Tree a
left (Node _ l _ _) = l

right :: Tree a -> Tree a
right (Node _ _ r _) = r

parent :: Tree a -> Tree a
parent (Node _ _ _ p) = p

insert' :: (Ord a) => Tree a -> Tree a -> a -> Tree a
insert' Leaf p v = Node v Leaf Leaf p
insert' t p v
    | v < (at t) = Node (at t) (insert' (left t) t v) (right t) p
    | v > (at t) = Node (at t) (left t) (insert' (right t) t v) p
    | otherwise = Node v (left t) (right t) p

getMin :: (Ord a) => Tree a -> a
getMin (Node v Leaf _ _) = v
getMin (Node _ l _ _) = getMin l

getMax :: (Ord a) => Tree a -> a
getMax (Node v _ Leaf _) = v
getMax (Node _ _ r _) = getMax r

changePar :: Tree a -> Tree a
changePar Leaf = Leaf
changePar t@(Node v Leaf Leaf p) = Node v Leaf Leaf p
changePar t@(Node v Leaf r p) = Node v Leaf (changePar (Node (at r) (left r) (right r) t)) p
changePar t@(Node v l Leaf p) = Node v (changePar (Node (at l) (left l) (right l) t)) Leaf p
changePar t@(Node v l r p) = Node v (changePar (Node (at l) (left l) (right l) t)) (changePar (Node (at r) (left r) (right r) t)) p


deleteThis :: (Ord a) => Tree a -> Tree a
deleteThis (Node _ Leaf Leaf _) = Leaf
deleteThis (Node _ Leaf r p) = Node (at r) (left r) (right r) p
deleteThis (Node _ l Leaf p) = Node (at l) (left l) (right l) p
deleteThis (Node _ l r p) = changePar (Node value l (delete r value) p) 
  where
    value = getMin r

findNode :: (Ord a) => Tree a -> Tree a -> a -> Tree a
findNode Leaf par val = par
findNode t@(Node v l r p) par val
    | val < v = findNode l t val
    | val == v = t
    | otherwise = findNode r t val

treeFold :: (Ord a) => (b -> a -> b) -> b -> Maybe a -> Tree a -> b
treeFold adder tmp Nothing tree = tmp
treeFold adder tmp val tree = adder (treeFold adder tmp (next tree (fromJust val)) tree) (fromJust val)

nextN :: (Ord a, Num a) => Tree a -> a -> a -> Maybe a
nextN t n v
    | n < 0 = Nothing
    | n == 0 = Just v
    | otherwise = (next t v) >>= nextN t (n-1)


instance Set Tree where
    emptySet = Leaf
    
    toList t = treeFold (\t v -> v : t) [] (Just (getMin t)) t 

    find Leaf v = False
    find t v
        | v == (at t) = True
        | v < (at t) = find (left t) v
        | otherwise = find (right t) v

    insert t v = insert' t Leaf v

    delete Leaf v = Leaf
    delete t v
        | v == (at t) = deleteThis t 
        | v < (at t) = Node (at t) (delete (left t) v) (right t) (parent t)
        | otherwise = Node (at t) (left t) (delete (right t) v) (parent t)

    next t v = next' (findNode t Leaf v) v
      where
        next' :: (Ord a) => Tree a -> a -> Maybe a
        next' (Node v _ Leaf p) val
            | v > val = Just v
            | otherwise = upNext p v
        next' (Node v _ r p) val 
            | v > val = Just v
            | otherwise = Just (getMin r)
        upNext :: (Ord a) => Tree a -> a -> Maybe a
        upNext Leaf val = Nothing
        upNext (Node v l r p) val
            | val < v = Just v
            | val > v = upNext p v

    prev t v = prev' (findNode t Leaf v) v
      where
        prev' :: (Ord a) => Tree a -> a -> Maybe a
        prev' (Node v Leaf _ p) val
            | v < val = Just v
            | otherwise = upPrev p v
        prev' (Node v l _ p) val 
            | v < val = Just val
            | otherwise = Just (getMax l)
        upPrev :: (Ord a) => Tree a -> a -> Maybe a
        upPrev Leaf val = Nothing
        upPrev (Node v l r p) val
            | val > v = Just v
            | val < v = upPrev p v

    fromList x = foldl insert Leaf x

instance (Set a, Ord b) => Monoid (a b) where
    mempty = emptySet
    mappend x y = foldl (insert) x (toList y)