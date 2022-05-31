
-- import Control.Monad.Writer
-- -- Learning about reader monads --
-- --  State s -> (a,s)
-- --  reader r -> a

-- type Value = Int

-- data Expr = Lit Value
--           | Add Expr Expr
--           | Mul Expr Expr
--           deriving (Show)

newtype ExceptT e m a = ExceptT (m (Either e a))

bindExceptT :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
bindExceptT mx f = do
  x <- mx -- `x` has the type `Either e a`
  case x of
    Left err -> pure (Left err)
    Right y -> f y
