{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Monads
import Prelude hiding (Monad(..))

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join a = undefined