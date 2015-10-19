{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Hw3.Task3.Monads
import Prelude hiding (Monad(..))

instance Monad m => MonadJoin m where
    returnJoin = return
    join a = a >>= id