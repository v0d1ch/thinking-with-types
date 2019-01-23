{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC  -fshow-hole-constraints #-}


module Irc where
---------------------------------------
  -- there is a significant improvement in performance
  -- if there is no indirection with creating a data structure
  -- just to throw it away, like with safeTail here

cpsTail :: [a] -> o -> ([a] -> o) -> o
cpsTail [] d = \f -> d
cpsTail (a:as) d = \f -> f as

cpsLoop :: (forall o. [a] -> o -> ([a] -> o) -> o) -> [a] -> [a]
cpsLoop f l = f l l $ cpsLoop f

-- this is an example with bad performance although the loop function
-- is not doing anything particularly useful, remember, this is just a playground
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

loop :: (a -> Maybe a) -> a -> a
loop f a = maybe a (loop f) (f a)
-----------------------------------------
class Functor m => Comonad m where
  extract :: m a -> a
  extend :: m a -> (m a -> b) -> m b

{- Laws:
extend extract = id
extract . extend f = f
extend f . extend g = extend (f . extend g)
-}

-- define some interesting Comonad instances
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store f' x) = Store (fmap f f') x

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s

  extend :: Store s a -> (Store s a -> b) -> Store s b
  extend (Store s a) f = Store (\x -> f (Store s a)) a


