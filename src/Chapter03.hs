module Chapter03 where
-- Variance


newtype T1 a = T1 (Int -> a)
-- regular functor , a appears in positive position
instance Functor T1 where
  fmap f (T1 f') = T1 $ fmap f f'

newtype T2 a = T2 (a -> Int)
-- a is in negative/contravariant position

newtype T3 a = T3 (a -> a)
-- this could be invariant functor since the thing we
-- want to map over has the same params both in covariant and
-- contravariant position

newtype T4 a = T4 ((Int -> a) -> Int)
-- here a is in negative position as well as the whole (Int -> a)
-- when composing/multiplying positive and negative positions we get
-- back a negative so we can not define lawful functor instance here

newtype T5 a = T5 ((a -> Int) -> Int)
-- here a appears in negative position and the whole expression (a -> Int)
-- also appears in negative position so two negatives composed yield back a positive
instance Functor T5 where
  fmap f (T5 aii) = T5 (\bi -> aii $ bi . f)


