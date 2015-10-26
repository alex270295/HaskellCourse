newtype Const a b = Const { getConst :: a }
newtype Identity a = Identity { runIdentity :: a }
{- Identity -}

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Functor Identity where
    fmap f x= Identity (f (runIdentity x))

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)

{- Either -}

instance Foldable (Either a) where
    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Applicative (Either a) where
    pure = Right
    Left f <*> _ = Left f
    Right f <*> x = fmap f x


{- Const -}

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

instance (Monoid m) => Applicative (Const m) where
    pure _ = Const mempty
    f <*> Const v = Const (f `mappend` v)


{- (,) a -}

instance Foldable ((,) a) where
    foldr f z (_, y) = f y z

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

instance (Monoid a) => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u `mappend` v, f x)

{- Tree -}

instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (map (fmapTree f) ts)

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Traversable Tree where
    traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts
