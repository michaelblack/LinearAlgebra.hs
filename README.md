A simple type safe linear algebra library. It's not the most efficient as it uses more or less a list based implementation, it was mainly used as a playground for DataKinds.

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


λ= eye #. (1 :- 2 :- 3 :- Scalar 4)
```
1 :- (2 :- (3 :- (4)))
````


λ= (pure 5 :: Matrix Three Two Int) #. (pure 2)
```
20 :- (20 :- (20))
```

