{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Algebra.Module where

import qualified Prelude as P
import Prelude (Integer, Int, Float, Double)

import Data.Algebra.Group
import Data.Algebra.Ring

class (Ring a, Group b) => LeftModule a b where
  (*>) :: a -> b -> b

class (Group a, Ring b) => RightModule a b where
  (<*) :: a -> b -> a