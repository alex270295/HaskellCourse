{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Monads
import Prelude hiding (Monad(..))

instance {-# OVERLAPS #-} MonadFish m => Monad m where
    return = returnFish
    (>>=) a f = (id >=> f) a