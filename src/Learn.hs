{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Learn
  ( pop,
    push,
    simulateStack',
    DelayedAction (DelayedAction),
    action,
    firstReader,
    secondReader,
    thirdReader,
    AbstractType,
  )
where

import Control.Concurrent
import Control.Monad (ap, join, liftM)
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.List

-----------------------------------
--- State Monad
-----------------------------------
pop :: State [Int] Int
pop = state (\(x : xs) -> (x, xs))

push :: Int -> State [Int] ()
push x = state (\xs -> ((), x : xs))

-- Monadic threading of the "stack",
-- implemented using the State monad.
simulateStack :: State [Int] Int
simulateStack = push 3 >> pop >>= (\x -> push (x * x)) >> pop

-- Do-syntax version of above; feels more
-- natural to imperative eyes.
-- Example in REPL: runState simulateStack' [1,2,3,4]
simulateStack' :: State [Int] Int
simulateStack' = do
  push 3
  x <- pop
  push (x * x)
  pop

newtype DelayedAction a = DelayedAction {action :: IO a}

instance Functor DelayedAction where
  fmap = liftM

instance Applicative DelayedAction where
  pure = return
  (<*>) = ap

instance Monad DelayedAction where
  return x = DelayedAction (return x)

  da >>= f =
    DelayedAction
      ( do
          firstIOValue <- action da
          threadDelay 1000000
          (action . f) firstIOValue
      )

-- De-sugared version of above!
-- da >>= f =
--   DelayedAction
--     ( action da >>= (\firstIOValue -> threadDelay 1000000 >> (action . f) firstIOValue)
--     )

-----------------------------------
--- Reader Monad
-----------------------------------
firstReader :: Reader [String] Int
firstReader = reader length

secondReader :: Reader [String] String
secondReader = reader head

thirdReader :: Reader [String] String
thirdReader = do
  env <- ask
  let render = intercalate "," env
  r1 <- firstReader
  r2 <- secondReader
  return $ "Env is of size " ++ show r1 ++ ", with head == " ++ r2 ++ ", Env == " ++ render

-----------------------------------
--- GADTs
-----------------------------------

-- A simple example: the constructors are given concrete types
data Expr a where
  III :: Int -> Expr Int
  BBB :: Bool -> Expr Bool

-----------------------------------
--- Type Families
-----------------------------------
type family AbstractType e a where
  AbstractType Int Bool = String
  AbstractType Bool Bool = Bool
