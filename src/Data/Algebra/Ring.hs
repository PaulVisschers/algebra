{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Algebra.Ring where

import qualified Prelude as P

import Data.Foldable
import Data.Algebra.Group

infixl 7 *, /

class Group a => Ring a where
  one :: a
  (*) :: a -> a -> a

instance P.Num a => Ring a where
  one = 1
  (*) = (P.*)

product :: (Foldable f, Ring a) => f a -> a
product = foldr (*) one

class Ring a => DivisionRing a where
  recip :: a -> a
  (/) :: a -> a -> a

instance P.Fractional a => DivisionRing a where
  recip = P.recip
  (/) = (P./)