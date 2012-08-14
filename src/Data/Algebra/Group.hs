{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Algebra.Group where

import qualified Prelude as P

import Data.Foldable

infixl 6 +, -

class Group a where
  zero :: a
  (+), (-) :: a -> a -> a
  negate :: a -> a

  negate x = zero - x
  x - y = x + negate y

sum :: (Foldable f, Group a) => f a -> a
sum = foldr (+) zero

instance P.Num a => Group a where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate