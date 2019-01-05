{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Chapter02 where

-- Exercise 2.1.3-i
-- If `Show Int` has kind Constraint, what's the kind of `Show` ?
-- Solution: Type -> Constraint

-- Exercise 2.1.3-ii
-- What is the kind of Functor
-- Solution: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iii
-- What is the kind of Monad
-- Solution: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iv
-- What is the kind of MonadTrans
-- Solution: (Type -> Type) -> Type -> Type -> Constraint

-- Exercise 2.4-i
-- Write a closed type family to compute not
type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True
