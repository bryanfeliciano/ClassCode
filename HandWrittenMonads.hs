{-# LANGUAGE OverloadedStrings #-}

type Log = [String]

newtype Logger a = Logger { runLogger :: (Log, a)}

addTwoM :: Int -> Logger Int
addTwoM x = Logger (["adding two..."], x + 2)

augmentAndStringifyM :: Int -> Int -> Logger String
augmentAndStringifyM x y =
  Logger (["augmenting..."], ()) >>= (\_ ->
    addTwoM x >>= (\x' ->
      addTwoM y >>= (\y' ->
        Logger (["stringifying..."], show (x' + y')))))

instance Functor Logger where
  fmap ab (Logger (l, a)) = Logger (l, ab a)

instance Applicative Logger where
  pure a = Logger ([], a)
  Logger (l, ab) <*> Logger (l', a) = Logger (l ++ l', ab a)

instance Monad Logger where
  Logger (l, a) >>= aLb = Logger (l ++ l', b)
    where
      (l', b) = runLogger . aLb $ a

log :: String -> Logger ()
log msg = Logger ([msg], ())

logs :: [String] -> Logger ()
logs msgs = Logger (msgs, ())

addTwo' :: Int -> Logger Int
addTwo' x = do
  log "adding two..."
  pure $ x + 2

augmentAndStringify' :: Int -> Int -> Logger String
augmentAndStringify' x y = do
  log "augmenting..."
  x' <- addTwo' x
  y' <- addTwo' y
  log "stringifying..."
  pure . show $ x' + y'

newtype Writer l a = Writer { runWriter :: (l, a) }

instance Functor (Writer l) where
  fmap ab (Writer la) = Writer $ ab <$> la

instance Monoid l => Applicative (Writer l) where
  pure a = Writer (mempty, a)
  Writer (l, ab) <*> Writer (l', a) = Writer (l <> l', ab a)

instance Monoid l => Monad (Writer l) where
  Writer (l, a) >>= aLb = Writer (l <> l', b)
    where
      (l', b) = runWriter . aLb $ a

tell :: l -> Writer l ()
tell l = Writer (l, ())

censor :: (l -> l) -> Writer l a -> Writer l a
censor f (Writer (l, a)) = Writer (f l, a)

listen :: Writer l a -> Writer l (a, l)
listen (Writer (l, a)) = Writer (l, (a, l))