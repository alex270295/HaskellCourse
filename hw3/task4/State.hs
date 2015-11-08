newtype State s a = State { runState :: s -> (a, s) }

instance Applicative (State s) where
    pure = undefined
    (<*>) = undefined

instance Functor (State s) where
    fmap = undefined

instance Monad (State s) where
    return a = State (\s -> (a, s))
    oldState >>= f = State (\s -> let (a, newState) = runState oldState s
                                   in runState (f a) newState)