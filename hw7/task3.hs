instance (Monad m) => Monad (ListT m) where
    return a = ListT $ return [a]
    m >>= k  = ListT $ do
        a <- runListT m
        b <- mapM (runListT . k) a
        return (concat b)

instance MonadTrans ListT where
    lift m = ListT $ do
        a <- m
        return [a]