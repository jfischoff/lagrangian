{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.AD.Lagrangian.Safe where
import Numeric.AD.Lagrangian.Internal (Constraint (..))
import qualified Numeric.AD.Lagrangian.Internal as I

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M

import GHC.IO (unsafePerformIO)

import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05
import Numeric.LinearAlgebra.Algorithms
import GHC.TypeLits
import Data.Traversable
import Data.Foldable
import Control.Applicative ((<$>))

data Proxy k = Proxy

data IList (argCount :: Nat) a = IList [a]

instance Functor (IList n) where
  fmap f (IList xs) = IList $ fmap f xs

instance Foldable (IList a) where
  foldMap f (IList xs) = foldMap f xs
  
instance Traversable (IList a) where
  traverse f (IList xs) = IList <$> traverse f xs

-- | Numerically minimize the Langrangian. The objective function and each of
-- the constraints must take the same number of arguments.
minimize :: forall (argCount :: Nat). 
            KnownNat argCount
         => 
            ( forall f a.
              ( Floating a
              , Traversable (f argCount)
              ) 
            => f argCount a -> a
            )
         -- ^ The objective function to minimize
         -> [Constraint]
         -- ^ A list of constraints @g \<=\> c@ corresponding to equations of
         -- the form @g(x, y, ...) = c@
         -> Double
         -- ^ Stop iterating when the largest component of the gradient is
         -- smaller than this value
         -> Either 
              (Result, Statistics) 
              (V.Vector Double, V.Vector Double)
         -- ^ Either a 'Right' containing the argmin and the Lagrange
         -- multipliers, or a 'Left' containing an explanation of why the
         -- gradient descent failed
minimize f constraints tolerance
  = I.minimize (f . IList) constraints tolerance (fromInteger $ natVal (Proxy :: Proxy argCount))

-- | Numerically maximize the Langrangian. The objective function and each of
-- the constraints must take the same number of arguments.
maximize :: forall (argCount :: Nat). 
            KnownNat argCount
         =>
            ( forall f a.
              ( Floating a
              , Traversable (f argCount)
              ) 
            => f argCount a -> a
            )
         -- ^ The objective function to minimize
         -> [Constraint]
         -- ^ A list of constraints @g \<=\> c@ corresponding to equations of
         -- the form @g(x, y, ...) = c@
         -> Double
         -- ^ Stop iterating when the largest component of the gradient is
         -- smaller than this value
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
         -- ^ Either a 'Right' containing the argmax and the Lagrange
         -- multipliers, or a 'Left' containing an explanation of why the
         -- gradient ascent failed
maximize f = minimize $ negate . f