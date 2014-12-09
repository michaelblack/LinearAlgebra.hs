LinearAlgebra.hs
=======================

A simple type safe linear algebra library. It's not the most efficient as it uses more or less a list based implementation.

It was mainly used as a playground for DataKinds and to see if I could make a neural network with typed input and output counts.

Many of the functions require a constraints such as (Applicative (Vector n)) because unfortunately GHC can not inductively deduce typeclass instances such as ∀n.Applicative (Vector n) from
```haskell
instance Applicative (Vector Z) where
  ....

instance Applicative (Vector n) => Applicative (Vector (S n)) where
  ....
```

Some nice use examples:

λ= eye :: Matrix Three Three Int
```
|	1	0	0	|
|				|
|	0	1	0	|
|				|
|	0	0	1	|
```


λ= pure 1 :: Vector Two Int
```
1 :- (1)
```


λ= :t (<#.>)
```
(<#.>) :: (Applicative (Vector n), Num a) => Matrix m n a -> Vector n a -> Vector m a
```


λ= eye <#.> (1 :- 2 :- 3 :- Scalar 4)
```
1 :- (2 :- (3 :- (4)))
````


λ= (pure 5 :: Matrix Three Two Int) <#.> (pure 2)
```
20 :- (20 :- (20))
```


λ= :t (<#>)
```
(<#>) :: (Applicative (Vector n), Num a) => Matrix m n a -> Matrix n o a -> Matrix m o a
```


λ= (pure :: Matrix Three Two Int) <#> eye
```
|	5	5	5	|
|				|
|	5	5	5	|
```


λ= eye <#> (pure :: Matrix Three Two Int)
```
|	5	5	5	|
|				|
|	5	5	5	|
```
