{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Monads
import Prelude hiding (Monad(..))

instance Monad m => MonadFish m where
    returnFish = return
    (>=>) f g = (\x -> returnFish x >>= f >>= g)