{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Hw3.Task3.Monads
import Prelude hiding (Monad(..))

instance Monad m => MonadFish m where
    returnFish = return
    (>=>) f g = (\x -> returnFish x >>= f >>= g)