{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LinearAlgebra where

import Control.Applicative hiding ((<|>))
import Data.Foldable
import Prelude hiding (foldr, foldr1, foldl, foldl1, sum)
import Data.Traversable
import Control.Comonad


data Nat = S Nat | One

type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four
type Six   = S Five
type Seven = S Six
type Eight = S Seven
type Nine  = S Eight
type Ten   = S Nine

infixr 5 :-
data Vector :: Nat -> * -> * where
  Scalar :: a -> Vector One a
  (:-) :: a -> Vector n a -> Vector (S n) a

data Matrix :: Nat -> Nat -> * -> * where
  Row   :: Vector m a -> Matrix One m a
  Above   :: Vector m a -> Matrix n m a -> Matrix (S n) m a

infixl 5 -:
(-:)  :: Vector n a -> a -> Vector (S n) a
(Scalar x) -: y = x :- (Scalar y)
(x :- xs)  -: y = x :- (xs -: y)

first :: Vector n a -> a
first (Scalar x) = x
first ( x :- _ ) = x

rest :: Vector (S n) a -> Vector n a
rest ( _ :- xs ) = xs

rowVector :: Vector n a -> Matrix One n a
rowVector xs = Row xs

columnVector :: Vector m a -> Matrix m One a
columnVector (x :- xs) = Above (Scalar x) (columnVector xs)
columnVector (Scalar x) = Row (Scalar x)

rows :: Matrix m n a -> Vector m (Vector n a)
rows (Row xs) =  Scalar xs
rows (Above r rs) = r :- rows rs

cols    :: Matrix m n a -> Vector n (Vector m a)
cols = rows . transpose

infixl 7 #.,.#
(#.) :: (Applicative (Vector n), Num a) => Matrix m n a -> Vector n a -> Vector m a
m #. v = first . cols $ m <> (columnVector v)

(.#) :: (Applicative (Vector m), Num a) => Vector m a -> Matrix m n a -> Vector n a
v .# m = first . rows $ (rowVector v) <> m

type family ( (n :: Nat) :+: (m :: Nat) ) :: Nat where
  (One   :+: m) = (S m)
  (S n) :+: m = S (n :+: m)



infixr 5 +++
(+++) :: Vector n a -> Vector m a -> Vector (n :+: m) a
(Scalar x)  +++ ys = x :- ys
(x:-xs) +++ ys = x :- (xs +++ ys)

infixr 5 <|>
(<|>) :: Matrix m n a -> Matrix m o a -> Matrix m (n :+: o) a
Row xs <|> Row ys = Row (xs +++ ys)
Above x xs <|> Above y ys = Above (x +++ y) (xs <|> ys)
infixr 5 <->
(<->) :: Matrix m n a -> Matrix o n a -> Matrix (m :+: o) n a
Row xs <-> ys = Above xs ys
Above x xs <-> ys = Above x (xs <-> ys)


infixl 7 <.>
(<.>) :: (Applicative (Vector n), Num a) => Vector n a -> Vector n a -> a
us <.> vs = sum $ (*) <$> us <*> vs

transpose :: Matrix m n a -> Matrix n m a
transpose (Row xs)        = columnVector xs
transpose (Above r rs   ) = columnVector r <|> transpose rs 

infixl 7 <>
(<>) :: (Applicative (Vector n), Num a) =>  Matrix m n a -> Matrix n o a -> Matrix m o a
m1 <> m2 = toMatrix ((\r -> (<.> r) <$> cols m2) <$> rows m1)

toMatrix :: Vector m (Vector n a) -> Matrix m n a
toMatrix (Scalar xs) = Row xs
toMatrix (x :- xs) = Above x (toMatrix xs)

instance Show a => Show (Vector n a) where
  show (Scalar x) = show x
  show (x :- xs) = show x ++ " :- (" ++ show xs ++ ")"

instance (Applicative (Vector n), Show a) => Show (Matrix m n a) where
  show (Row xs) = (foldl' (\acc u -> acc ++ "\t" ++ show u) "|" xs) ++ "\t|\n"
  show (Above r rs) = (foldl' (\acc u -> acc ++ "\t" ++ show u) "|" r) ++ "\t|\n|" ++ foldr (:) "" (pure '\t' :: Vector n Char) ++  "\t|\n" ++ show rs

instance Foldable (Vector n) where
  foldr step acc (Scalar x) = step x acc
  foldr step acc (x :- xs ) = step x (foldr step acc xs)

instance Foldable (Matrix m n) where
  foldr step acc (Row xs) = foldr step acc xs
  foldr step acc (Above x xs) = foldr step (foldr step acc xs) x

instance Traversable (Vector n) where
  traverse f (Scalar x) = Scalar <$> f x
  traverse f (x :- xs ) = (:-) <$> f x <*> traverse f xs
  
instance Functor (Vector n) where
  fmap f (Scalar x) = Scalar (f x)
  fmap f (x:-xs) = f x :- fmap f xs

instance Applicative (Vector One) where
  pure = Scalar
  (Scalar f) <*> (Scalar x) = Scalar (f x)

instance Applicative (Vector n) => Applicative (Vector (S n)) where
  pure f  = f :- pure f
  (f :- fs) <*> (x :- xs) = f x :- (fs <*> xs)

shift :: Vector n a -> Vector n a
shift (Scalar x) = Scalar x
shift (x :- xs)  = xs -: x

instance Comonad (Vector One) where
  extract (Scalar x) = x
  duplicate = Scalar
  
instance IterV n => Comonad (Vector  n) where
  extract (x :- _ ) = x
  duplicate xs = iterV shift xs

class IterV (n :: Nat) where
      iterV ::  (a -> a) -> a -> Vector n a

instance IterV One where
  iterV _ x = (Scalar x)

instance IterV n => IterV (S n) where
  iterV f x = x :- iterV f (f x)
  
instance Functor (Matrix n m) where
  fmap f (Row xs) = Row (fmap f xs)
  fmap f (Above x xs) = Above (fmap f x) (fmap f xs)


instance  Applicative (Vector n) => Applicative (Matrix One n) where
  pure  = Row . pure 
  (Row fs) <*> (Row xs) = Row (fs <*> xs)

instance (Applicative (Vector n), Applicative (Matrix m n)) => Applicative (Matrix (S m) n) where
  pure f = Above (pure f) (pure f)
  (Above f fs) <*> (Above x xs) = Above (f <*> x) (fs <*> xs)

class Eye (n :: Nat) where
  eye :: Num a => Matrix n n a

instance Eye One where
  eye = Row (Scalar 1)

instance (Applicative (Vector n), Eye n) => Eye (S n) where
  eye = Above (1 :- pure 0) (columnVector (pure 0) <|> (eye :: Num a => Matrix n n a))

