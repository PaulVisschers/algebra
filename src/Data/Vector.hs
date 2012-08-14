{-# LANGUAGE EmptyDataDecls, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, OverlappingInstances, DataKinds, KindSignatures #-}
module Data.Vector where

import qualified Prelude as P
import Prelude (Eq, Show, Ord, Ordering (..), compare, Num, Functor, (==), (.), show, fmap, error, Int, (&&), Bool (..))
import Data.Foldable (Foldable, foldr, toList)

import Data.Algebra

data Natural = Zero | Succ Natural

data Vector (n :: Natural) a where
  Nil :: Vector Zero a
  Cons :: Nat n => a -> Vector n a -> Vector (Succ n) a

instance Eq a => Eq (Vector n a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys

instance Ord a => Ord (Vector n a) where
  compare Nil Nil = EQ
  compare (Cons x xs) (Cons y ys) = if ord == EQ then compare xs ys else ord where
    ord = compare x y

instance Show a => Show (Vector n a) where -- Rudimentary for debugging, clean up.
  show = show . toList

snoc :: a -> Vector n a -> Vector (Succ n) a
snoc x Nil = Cons x Nil
snoc x (Cons y v) = Cons y (snoc x v)

head :: Vector (Succ n) a -> a
head (Cons x _) = x

tail :: Vector (Succ n) a -> Vector n a
tail (Cons _ xs) = xs

last :: Vector (Succ n) a -> a
last (Cons x Nil) = x
last (Cons _ (Cons x v)) = last (Cons x v)

init :: Vector (Succ n) a -> Vector n a
init (Cons _ Nil) = Nil
init (Cons x (Cons y v)) = Cons x (init (Cons y v))

length :: (P.Floating a, Ring a) => Vector n a -> a
length v = P.sqrt (dot v v)

normalize :: (Eq a, P.Floating a, DivisionRing a, Nat n) => Vector n a -> Vector n a
normalize v = if l == zero then zero else fmap (/ l) v where
  l = length v

zip :: Vector n a -> Vector n b -> Vector n (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

zipWith4 :: (a -> b -> c -> d -> e) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e
zipWith4 _ Nil Nil Nil Nil = Nil
zipWith4 f (Cons x xs) (Cons y ys) (Cons z zs) (Cons w ws) = Cons (f x y z w) (zipWith4 f xs ys zs ws)

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e -> Vector n f
zipWith5 _ Nil Nil Nil Nil Nil = Nil
zipWith5 f (Cons x xs) (Cons y ys) (Cons z zs) (Cons w ws) (Cons v vs) = Cons (f x y z w v) (zipWith5 f xs ys zs ws vs)

unzip :: Vector n (a, b) -> (Vector n a, Vector n b)
unzip Nil = (Nil, Nil)
unzip (Cons (x, y) xys) = (Cons x xs, Cons y ys) where 
  (xs, ys) = unzip xys

instance Functor (Vector n) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable (Vector n) where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance (Group a, Nat n) => Group (Vector n a) where
  zero = repeat zero
  (+) = zipWith (+)
  (-) = zipWith (-)
  negate = fmap negate

instance (Nat n, Ring a) => LeftModule a (Vector n a) where
  x *> y = fmap (x *) y

instance (LeftModule a b, Nat n) => LeftModule a (Vector n b) where
  x *> y = fmap (x *>) y

dot :: (Ring a) => Vector n a -> Vector n a -> a
dot x y = sum (zipWith (*) x y)

class Nat n where
  --vectorFix :: (forall m. Nat m => Vector m a -> Vector (Succ m) a) -> Vector n a
  unfoldr :: (b -> (a, b)) -> b -> Vector n a

instance Nat Zero where
  --vectorFix f = Nil
  unfoldr f x = Nil

instance Nat n => Nat (Succ n) where
  --vectorFix f = f (vectorFix f)
  unfoldr f x = Cons y (unfoldr f z) where
    (y, z) = f x

repeat :: Nat n => a -> Vector n a
repeat = unfoldr (\x -> (x,x))

iterate :: Nat n => (a -> a) -> a -> Vector n a
iterate f = unfoldr (\x -> (x, f x))

type Matrix n m a = Vector n (Vector m a)

class Multiplicative a b c | a b -> c where
  (><) :: a -> b -> c

instance Ring a => Multiplicative (Matrix n m a) (Vector m a) (Vector n a) where
  m >< v = fmap (dot v) m

instance (Nat o, Ring a) => Multiplicative (Matrix n m a) (Matrix m o a) (Matrix n o a) where
  a >< b = fmap (transpose b ><) a

instance (Nat n, Ring a) => Ring (Matrix n n a) where
  one = unfoldr f 0 where
    f :: (Nat n, Ring a) => Int -> (Vector n a, Int)
    f n = (unfoldr (\(n, x) -> if n == x then (one, (n, x + 1)) else (zero, (n, x + 1))) (n, 0), n + 1)
  a * b = a >< b

instance (Nat n, Ring a) => LeftModule (Matrix n n a) (Vector n a) where
  m *> v = m >< v

transpose :: Nat m => Matrix n m a -> Matrix m n a
transpose Nil = repeat Nil
transpose (Cons Nil _) = Nil
transpose xs@(Cons (Cons _ _) _) = Cons (fmap head xs) (transpose (fmap tail xs))

dual :: Group a => Vector3 a -> Vector3 (Vector3 a)
dual (Cons x (Cons y (Cons z Nil))) = vector3 x' y' z' where
  x' = vector3 zero (negate z) y
  y' = vector3 z zero (negate x)
  z' = vector3 (negate y) x zero

cross :: Ring a => Vector3 a -> Vector3 a -> Vector3 a
cross a b = dual a >< b


type Vector1 = Vector (Succ Zero)
type Vector2 = Vector (Succ (Succ Zero))
type Vector3 = Vector (Succ (Succ (Succ Zero)))
type Vector4 = Vector (Succ (Succ (Succ (Succ Zero))))

vx :: Vector n a -> a
vx (Cons x _) = x

vy :: Vector (Succ n) a -> a
vy (Cons _ (Cons y _)) = y

vz :: Vector (Succ (Succ n)) a -> a
vz (Cons _ (Cons _ (Cons z _))) = z

vector1 :: a -> Vector1 a
vector1 x = Cons x Nil

vector2 :: a -> a  -> Vector2 a
vector2 x y = Cons x (Cons y Nil)

vector3 :: a -> a -> a -> Vector3 a
vector3 x y z = Cons x (Cons y (Cons z Nil))

vector4 :: a -> a -> a -> a -> Vector4 a
vector4 x y z w = Cons x (Cons y (Cons z (Cons w Nil)))