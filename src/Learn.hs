module Learn
  ( pop,
    push,
    simulateStack',
    DelayedAction (DelayedAction),
    getInsideAction,
  )
where

import Control.Concurrent
import Control.Monad (ap, liftM)
import Control.Monad.Trans.State

pop :: State [Int] Int
pop = state (\(x : xs) -> (x, xs))

push :: Int -> State [Int] ()
push x = state (\xs -> ((), x : xs))

-- Monadic threading of the "stack",
-- implemented using the State monad.
simulateStack :: State [Int] Int
simulateStack = (push 3) >> pop >>= (\x -> push (x * x)) >> pop

-- Do-syntax version of above; feels more
-- natural to imperative eyes.
-- Example in REPL: runState simulateStack' [1,2,3,4]
simulateStack' :: State [Int] Int
simulateStack' = do
  push 3
  x <- pop
  push (x * x)
  pop

newtype DelayedAction a = DelayedAction (IO a)

getInsideAction :: DelayedAction a -> IO a
getInsideAction (DelayedAction x) = x

instance Functor DelayedAction where
  fmap = liftM

instance Applicative DelayedAction where
  pure = return
  (<*>) = ap

instance Monad DelayedAction where
  return x = DelayedAction (threadDelay 1000000 >> return x)

  (DelayedAction firstIO) >>= f = undefined

  (DelayedAction firstIO) >> (DelayedAction secondIO) =
    DelayedAction
      ( do
          firstIO
          threadDelay 1000000
          secondIO
      )
