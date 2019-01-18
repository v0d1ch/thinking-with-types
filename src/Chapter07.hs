{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fshow-hole-constraints #-}
module Chapter07 where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }
-- Exercise 6.4-i
-- Provide Functor instance for Cont
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont c) = Cont (\x -> c $ x . f)

instance Functor (ContT m) where
  fmap :: (a -> b) -> ContT m a -> ContT m b
  fmap f (ContT c) = ContT (\x -> c $ x . f)

-- Exercise 6.4-ii
-- Provide Applicative instance for Cont
instance Applicative Cont where
  pure :: a -> Cont a
  pure c = Cont (\a -> a c)

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) (Cont f) (Cont a) =
    Cont $ \br ->
      f $ \ab ->
        a $ br . ab

instance Applicative (ContT m) where
  pure :: a -> ContT m a
  pure c = ContT (\a -> a c)

  (<*>) :: ContT m (a -> b) -> ContT m a -> ContT m b
  (<*>) (ContT f) (ContT a) =
    ContT $ \br ->
      f $ \ab ->
        a $ br . ab

-- Exercise 6.4-iii
-- Provide Monad instance for Cont
instance Monad Cont where
  return = pure
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  Cont m >>= f = Cont $ \x ->
    -- m :: (a -> r) -> r
    -- f :: a -> Cont (a -> r) -> r
    -- y :: (a -> r)
    -- x :: (b -> r)
    -- f y :: Cont ((a -> r) -> r1) -> r1
    m (\y -> unCont (f y) x)

instance Monad (ContT m) where
  return = pure
  (>>=) :: ContT m a -> (a -> ContT m b) -> ContT m b
  ContT m >>= f = ContT $ \x ->
    m (\y -> unContT (f y) x)
