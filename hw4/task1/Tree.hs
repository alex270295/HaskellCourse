{-# LANGUAGE FlexibleInstances #-}

type LeftTree = Tree
type RightTree = Tree
data Tree a = Leaf | Node a (LeftTree a) (RightTree a) deriving (Show)


instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node v l r) = (f v) `mappend` (foldMap f l) `mappend` (foldMap f r)

instance Applicative Tree where
    pure x = Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node f l1 r1) <*> (Node v l2 r2) = Node (f v) (l1 <*> l2) (r1 <*> r2)

instance Traversable Tree where
    traverse f (Node v l r) = Node <$> (f v) <*> (traverse f l) <*> (traverse f r)

