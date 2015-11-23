instance (Monad m) => Monad (StateT s m) where
    return a = state $ \ s -> (a, s)
    m >>= k  = StateT $ \ s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'

instance MonadTrans (StateT s) where
	lift m = StateT $ \ s -> do
		a <- m
		return (a, s)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
	return a = writer (a, mempty)
	m >>= k  = WriterT $ do
		(a, w)  <- runWriterT m
		(b, w') <- runWriterT (k a)
		return (b, w `mappend` w')

instance (Monoid w) => MonadTrans (WriterT w) where
	lift m = WriterT $ do
		a <- m
		return (a, mempty)

instance Monad m => Monad (EitherT l m) where
  return = EitherT . return . Right
	EitherT x >>= f = EitherT $ do
		res <- x
		case res of
			Right r -> runEitherT . f $ r
			Left l -> return (Left l)

instance MonadTrans (EitherT l) where
  lift = EitherT . liftM Right

