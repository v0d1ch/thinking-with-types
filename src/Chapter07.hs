{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Chapter07 where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

-- Exercise 6.4-i
-- Provide Functor instance for Cont
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont c) = Cont (\x -> c $ x . f)

-- Exercise 6.4-ii
-- Provide Applicative instance for Cont
instance Applicative Cont where
  pure :: a -> Cont a
  pure c = Cont (\a -> a c)

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) (Cont f) (Cont a) =
    -- Cont (\br -> br $ f a)
    Cont $ \br ->
      f $ \ab ->
        a $ br . ab

-- Exercise 6.4-iii
-- Provide Monad instance for Cont
instance Monad Cont where
  return = pure
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  Cont m >>= f = Cont $ undefined
