{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}
{-# OPTIONS_GHC  -Wall -fshow-hole-constraints #-}


module Irc where
import Data.Functor.Identity
import Data.Functor.Const
---------------------------------------
  -- there is a significant improvement in performance
  -- if there is no indirection with creating a data structure
  -- just to throw it away, like with safeTail here

cpsTail :: [a] -> o -> ([a] -> o) -> o
cpsTail [] d = \_ -> d
cpsTail (_:as) _ = \f -> f as

cpsLoop :: (forall o. [a] -> o -> ([a] -> o) -> o) -> [a] -> [a]
cpsLoop f l = f l l $ cpsLoop f

-- this is an example with bad performance although the loop function
-- is not doing anything particularly useful, remember, this is just a playground
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

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
-- <dminuoso> v0d1ch: A Store is a generalized indexed container that has a
-- "current position"
-- <dminuoso> v0d1ch: So in a store you can "read out an arbitrary position",
-- "read out the current position" or "seek to a new position"
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store f' x) = Store (fmap f f') x

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s

  -- "replacing all values with containers, and then consuming those containers again"
  extend :: Store s a -> (Store s a -> b) -> Store s b
  extend s f = fmap f $ duplicate s
   -- verbose: Store (\x -> f' $ Store f x) s

duplicate :: Store s a -> Store s (Store s a)
duplicate (Store f s) = Store (\x -> Store f x) s

-- Here's a couple of "standard store functions" you might want to implement.
-- https://gist.github.com/dminuoso/ac6d72cf8d83d96b84ecdc23ed44cae2 ->
-- try them out to get a feeling for them, and then implement the entire Comonad
-- instance using those utility functions.
-- 9:01 AM <dminuoso> v0d1ch: Also note that the last utility function
-- `experiment` is included for completeness and will play a really interesting
-- role in the next excercise. :)

-- Get the store focus
pos :: Store s a -> s
pos (Store _ a) = a

-- Read out the store at some specific position
peek :: s -> Store s a -> a
peek s (Store f _) = f s

-- Modify the current focus, and read the store using the new focus.
peeks :: (s -> s) -> Store s a -> a
peeks f (Store f' s) = f' (f s)

-- Set the current focus
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

-- Modify the current focus
seeks :: (s -> s) -> Store s a -> Store s a
seeks f (Store f' s) = Store f' (f s)

-- Run an experiment in the store.
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store s a) = fmap s (f a)

-- So take
-- 16:12 experiment :: Functor f => (s -> f s) -> Store s a -> f a
-- 16:12 The original definition of Store was:
-- 16:12 data Store s a = Store (s -> a) s
-- 16:12 An alternative definition is:
-- 16:13 newtype Pretext s a = Pretext { runPretext :: forall f. Functor f => (s -> f s) -> f a }
-- 16:13 Which can be thought of as `experiment` partially applied to a Store.
-- 16:13 Turns out that this representation is 100% equivalent. =)
-- 16:13 <v0d1ch> Sasa Bogicevic I see
-- 16:14
-- <dminuoso> Just ask If you enabled DerivingFunctor you could write this as:
-- If you want a real good challenge, try implementing `instance Comonad (Pretext s)
-- 16:15 <dminuoso> The elegant form can be found if you define it in terms of extract/duplicate rather than extract/extend.
-- 16:15 <dminuoso> But it's tricky to find.

newtype Pretext s a = Pretext { runPretext :: forall f. Functor f => (s -> f s) -> f a } deriving Functor

-- 15:45 Now I want you to implement: Functor, Applicative and Traversable.
--
data Vec3 a = Vec3 a a a deriving (Eq, Show)

instance Functor Vec3 where
  fmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)

instance Applicative Vec3 where
  pure a = Vec3 a a a
  (Vec3 f f' f'') <*> (Vec3 a b c)  =  Vec3 (f a) (f' b) (f'' c)

instance Foldable Vec3 where
   foldr :: (a -> b -> b) -> b -> Vec3 a -> b
   foldr f d (Vec3 a b c) = f a (f b (f c d))

instance Traversable Vec3 where
  traverse :: Applicative f => (a -> f b) -> Vec3 a -> f (Vec3 b)
  traverse f (Vec3 a b c) = Vec3 <$> f a <*> f b <*> f c

-- traverseX :: Applicative f => (a -> f a) -> Vec3 a -> f (Vec3 a)
-- traverseX f (Vec3 a b c) = Vec3 <$> f a <*> pure b <*> pure c
-- relax the constraint to Functor by applying the laws:
-- Applicative laws:
-- identity
--     pure id <*> v = v
-- composition
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- homomorphism
--     pure f <*> pure x = pure (f x)
-- interchange
--     u <*> pure y = pure ($ y) <*> u
-- The other methods have the following default definitions, which may be
-- overridden with equivalent specialized implementations:
--     u *> v = (id <$ u) <*> v
--     u <* v = liftA2 const u v
-- As a consequence of these laws, the Functor instance for f will satisfy
--     fmap f x = pure f <*> x
-- It may be useful to note that supposing
-- forall x y. p (q x y) = f x . g y
-- it follows from the above that
-- liftA2 p (liftA2 q u v) = liftA2 f u . liftA2 g v
--
traverseA :: Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)
traverseA f (Vec3 a b c) = (\x -> Vec3 x b c) <$> f a
-- fmap ((\g' g'' y -> g' (g'' y)) ((\f' f'' x -> f' (f'' x)) (\c' -> c' c) (\b' -> b' b)) Vec3) (f a)
-- ($ c) . ($ b) . Vec3
-- (\x -> Vec3 x b c) <$> (f a)

traverseB :: Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)
traverseB f (Vec3 a b c) = (\x -> Vec3 a x c) <$> f b

traverseC :: Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)
traverseC f (Vec3 a b c) = (\x -> Vec3 a b x) <$> f c

traverseL :: Functor f => ((a -> f a) -> Vec3 a -> f (Vec3 a)) -> (a -> f a) -> Vec3 a -> f (Vec3 a)
traverseL f f' v = f f' v

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

over :: (forall f. Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)) -> (a -> a) -> Vec3 a -> Vec3 a
over f f' v = runIdentity $ f (Identity . f') v

get :: Lens s a -> a -> s
get l d = getConst $ (l Const) d

