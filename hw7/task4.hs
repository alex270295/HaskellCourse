instance (Monad m) => MonadState s (StateT s m) where
    get   = state $ \ s -> (s, s)
    put s = state $ \ _ -> ((), s)