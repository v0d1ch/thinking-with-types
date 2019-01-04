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
-}

