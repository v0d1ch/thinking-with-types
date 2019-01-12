{-# LANGUAGE RankNTypes #-}
module Chapter07 where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap f (Cont c) = Cont (\x -> c $ x . f)
