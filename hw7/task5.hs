import Control.Monad.State

state :: MonadState s m => (s -> (a, s)) -> m a
state f = do
      s <- get
      let (a, s') = f s
      put s'
      return a