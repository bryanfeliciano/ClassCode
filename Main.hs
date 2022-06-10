-- import Control.Monad.Writer
-- -- Learning about reader monads --
-- --  State s -> (a,s)
-- --  reader r -> a

-- State implemented in functors,applicatives and monad

type Count = Int

-- We can use this to construct a type:
newtype State s a = State {runState :: s -> (s, a)}

-- We'll assume our Functor, Applicative, and Monad instances already exist, and rewrite our code:
reverseWithCountM :: [a] -> State Count [a]
reverseWithCountM list = State (\count -> (count + 1, reverse list))

-- The function no longer takes `count` as a parameter: it's now embedded inside State

appendReversedWithCountM :: [a] -> [a] -> State Count [a]
appendReversedWithCountM list1 list2 =
  reverseWithCountM list1
    >>= ( \revList1 ->
            reverseWithCountM list2
              >>= ( \revList2 ->
                      State (\count -> (count + 1, revList1 ++ revList2))
                  )
        )

-- The `count` variable is being updated and threaded through the nested function calls...
-- but it is abstracted away so we don't need to explicitly manage it until the final step.

-- When we implement the Monad instance and define the bind function we will handle the state there

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap ab (State sa) =
    State
      ( \s ->
          let (s', a) = sa s
           in (s', ab a)
      )

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (s, a))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State sab <*> State sa =
    State
      ( \s ->
          let (s', ab) = sab s
              (s'', a) = sa s'
           in (s'', ab a)
      )

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= aSsb =
    State
      ( \s ->
          let (s', a) = sa s
           in runState (aSsb a) s'
      )

newtype Logger a = Logger {runLogger :: (Log a)}


