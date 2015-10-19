{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Monads
import Prelude hiding (Monad(..))

instance Monad m => MonadJoin m where
    returnJoin = return
    join a = a >>= id