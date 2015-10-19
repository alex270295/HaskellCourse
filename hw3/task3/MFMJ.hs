{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import Haskell.Hw3.Task3.Monads
import Prelude hiding (Monad(..))

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = (id >=> id)

--for testing on Maybe
instance Monad m => MonadFish m where
    returnFish = return
    (>=>) f g = (\x -> returnFish x >>= f >>= g)