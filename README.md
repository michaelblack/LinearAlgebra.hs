LinearAlgebra.hs
=======================

A simple type safe linear algebra library. It's not the most efficient as it uses more or less a list based implementation.

It was mainly used as a playground for DataKinds and to see if I could make a neural network with typed input and output counts.

Many of the functions require a constraints such as (Applicative (Vector n)) because unfortunately GHC can not inductively deduce typeclass instances such as ∀n.Applicative (Vector n) from
```haskell
instance Applicative (Vector One) where
  ....

instance Applicative (Vector n) => Applicative (Vector (S n)) where
  ....
```

Some nice use examples:

λ= eye :: Matrix Three Three Int -- The Identity Matrix 
```
|	1	0	0	|
|				|
|	0	1	0	|
|				|
|	0	0	1	|
```


λ= pure 1 :: Vector Two Int -- The Applicative Instance of a Vector
```
1 :- (1)
```


λ= liftA2 (+) (pure 1 :: Vector Two Int) (pure 3) -- (<*>) is pointwise
```
4 :- (4)
``` 


λ= :t (<#.>) -- Matrix * Vector multiplication
```
(<#.>) :: (Applicative (Vector n), Num a) => Matrix m n a -> Vector n a -> Vector m a
```


λ= eye <#.> (1 :- 2 :- 3 :- Scalar 4) -- Notice the type of 'eye' is inferred
```
1 :- (2 :- (3 :- (4)))
````


λ= (pure 5 :: Matrix Three Two Int) <#.> (pure 2)
```
20 :- (20 :- (20))
```


λ= :t (<#>) -- Matrix * Matrix multiplication
```
(<#>) :: (Applicative (Vector n), Num a) => Matrix m n a -> Matrix n o a -> Matrix m o a
```


λ= (pure 5 :: Matrix Three Two Int) <#> eye -- Notice again that the type of eye is inferred
```
|	5	5	5	|
|				|
|	5	5	5	|
```


λ= eye <#> (pure 5 :: Matrix Three Two Int) -- (eye <#>) == (<#> eye) == id
```
|	5	5	5	|
|				|
|	5	5	5	|
```
