module Chapter01 where

{-
Chapter 01

When talking about the algebra of types we can talk about isomorphisms and
cardinality of types. Cardinality is determined by the number of inhabitants
for type (ignoring bottoms).
So for:
  |data Void|              = 0
  |data Unit = ()|         = 1
  |data Bool = True|False| = 2

We can say that any two types whose cardinality is the same are isomorphic to
each other. Which isomorphism we pick is not important, we just need to know
if the isomorphism exists.

For Product types we determine the cardinality by multiplaying the inhabitants
 so:
 (a,b) = |a| x |b|

Cardinality enables us to expres mathematical truths about types. We are able to
prove that a x 1 = a by showing the isomorphism between (a, ()) = a

  > prodUnitTo :: a -> (a, ())
  > prodUnitTo a = (a,())


  > prodUnitFrom :: (a, ()) -> a
  > prodUnitFrom (a, ()) = a

Here () is a monoidal identity for product types. Adding it in does not change
anything much like multiplaying with 1 (which is the ()'s cardinality) does
not affect the result.

For Sum types monoidal identity would be Void (cardinality 0) since a + 0 = a

Function types have the cardinality of:

  a -> b = |b| to the power of a

for any a we need to produce the appropriate b so we will end up with b to the
power of a number of types.

Exercise 1.2-i
--------------
Determine the cardinality of

  Either Bool (Bool,Maybe Bool) -> Bool

Solution:
  2 + (2 x (1 + 2)) x (2^8) = 256

Exercise 1.4-i
--------------
Use Curry-Howard to prove that
  (a^b)^c  = a^(b x c)
That is, provide a function of type
  > (b -> c -> a) -> (b,c) -> a
  > ((b -> c) -> a) -> b -> c -> a
Do these functions remind you of anything from Prelude ?

Solution:

ex :: (b -> c -> a) -> (b,c) -> a
ex f (b,c) = f (b,c)

ex1 :: ((b -> c) -> a) -> b -> c -> a
ex1 f b c = f b c

So cardinality of function (a -> b) (one argument)  is  b^a
cardinality of function (a -> b -> c) would then be c^(b^a)
if we specialise the types to Bool then we end up with
 (Bool -> Bool -> Bool) = 2^(2^2)
2^(2^2) == 2 (2 x 2) = 2^4

uncurry/curry from Prelude


Exercise 1.4-ii
--------------
  Give a proof of the exponent law that a^b x a^c = a ^(b + c)
exTo :: (a -> c) -> (b -> c) -> Either a b -> c
exTo f _ (Left a) = f a
exTo _ f (Right b) = f' b

exFrom :: (Either a b -> c) -> (a -> c, b -> c)
exFrom f = (f . Left, f . Right)

-}

