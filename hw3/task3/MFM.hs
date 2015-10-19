{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Hw3.Task3.Monads
import Prelude hiding (Monad(..))

instance {-# OVERLAPS #-} MonadFish m => Monad m where
    return = returnFish
    (>>=) a f = (id >=> f) a