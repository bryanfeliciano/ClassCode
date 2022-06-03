
-- import Control.Monad.Writer
-- -- Learning about reader monads --
-- --  State s -> (a,s)
-- --  reader r -> a

bindExceptT :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
bindExceptT mx f = do
  x <- mx -- `x` has the type `E e a`
  case x of
    Left err -> pure (Left err)
    Right y -> f y

-- Implementation of state and Io together

type World s m a  = s -> m  (a,s) 
type StateIO s a = s -> IO (s,a)

-- return for a custom state would look something like this 
-- returnStateIO :: a -> (State s) a 