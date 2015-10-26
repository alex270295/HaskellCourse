newtype Const a b = Const { getConst :: a }
newtype Identity a = Identity { runIdentity :: a }
data Pair a b = First a | Second b

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

{- Pair (Either)-}

instance Foldable (Pair a) where
    foldr _ z (First _) = z
    foldr f z (Second y) = f y z

instance Functor (Pair a) where
    fmap _ (First x) = First x
    fmap f (Second y) = Second (f y)

instance Traversable (Pair a) where
    traverse _ (First x) = pure (First x)
    traverse f (Second y) = Second <$> f y

instance Applicative (Pair a) where
    pure = Second
    First f <*> _ = First f
    Second f <*> x = fmap f x


{- Const -}

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

instance (Monoid m) => Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f `mappend` v)

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