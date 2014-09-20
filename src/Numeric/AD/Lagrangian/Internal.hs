{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Numeric.AD.Lagrangian.Internal where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Packed.Vector as V
--import qualified Data.Packed.Matrix as M

import Data.Reflection

import GHC.IO (unsafePerformIO)

import Numeric.AD
import Numeric.AD.Internal.Reverse
--import Numeric.AD.Internal.Sparse
--import Numeric.AD.Internal.On

import Numeric.Optimization.Algorithms.HagerZhang05
--import Numeric.LinearAlgebra.Algorithms

type RR s s' a = Reverse s (Reverse s' a)

newtype Constraint a = Constraint
  { unConstraint :: forall s s'. (Reifies s Tape, Reifies s' Tape)
                 => ([RR s s' a] -> RR s s' a, Scalar (Scalar (RR s s' a)))
  }

(<=>) :: (forall s s'. (Reifies s Tape, Reifies s' Tape)
          => [RR s s' a]
          -> RR s s' a)
      -> a
      -> Constraint a
f <=> c = Constraint (f, c)

minimize :: (forall s s'. (Reifies s Tape, Reifies s' Tape)
             => [RR s s' Double]
             -> RR s s' Double)
         -> [Constraint Double]
         -> Double
         -> Int
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
minimize f constraints tolerance argCount = result where
    -- At a constrained minimum of `f`, the gradient of the Lagrangian must be
    -- zero. So we square the Lagrangian's gradient (making it non-negative) and
    -- minimize that.
    (sqGradLgn, gradSqGradLgn) = (fst . g, snd . g) where
        g = grad' $ squaredGrad $ lagrangian f constraints argCount
    
    -- Perhaps this should be exposed. ...
    guess = U.replicate (argCount + length constraints) 1

    result = case unsafePerformIO $
                    optimize
                        (defaultParameters {printFinal = False})
                        tolerance
                        guess
                        (VFunction (sqGradLgn . U.toList))
                        (VGradient (U.fromList . gradSqGradLgn . U.toList))
                        Nothing of
       (vs, ToleranceStatisfied, _) -> Right (S.take argCount vs,
                                              S.drop argCount vs)
       (_, x, y) -> Left (x, y)

maximize :: (forall s s'. (Reifies s Tape, Reifies s' Tape)
             => [RR s s' Double]
             -> RR s s' Double)
         -> [Constraint Double]
         -> Double
         -> Int
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
maximize f = minimize $ negate . f

lagrangian :: (Num a, Reifies s Tape, Reifies s' Tape)
           => ([RR s s' a] -> RR s s' a)
           -> [Constraint a]
           -> Int
           -> [RR s s' a]
           -> RR s s' a
lagrangian f constraints argCount argsAndLams = result where
    args = take argCount argsAndLams
    lams = drop argCount argsAndLams

    -- g(x, y, ...) = c <=> g(x, y, ...) - c = 0
    appliedConstraints = fmap (\(Constraint (g, c)) -> g args - (auto $ auto c)) constraints

    -- L(x, y, ..., lam0, ...) = f(x, y, ...) + lam0 * (g0 - c0) ...
    result = (f args) + (sum . zipWith (*) lams $ appliedConstraints)

squaredGrad :: (Num a)
            => (forall s. (Reifies s Tape) => [Reverse s a] -> Reverse s a)
            -> [a]
            -> a
squaredGrad f = sum . fmap square . grad f where
    square x = x * x

--------------------------------------------------------------------------------
-- Try, e.g., `minimize entropy [sum <=> 1] (1e-10) 3` to verify that all works.
--------------------------------------------------------------------------------

entropy :: (Floating a) => [a] -> a
entropy = negate . sum . fmap (\x -> x * log x)

probSum :: (Num a, Num b) => ([a] -> a, b)
probSum = (sum, 1)

--------------------------------------------------------------------------------
-- Temporary stubs for code imported by Numeric.AD.Lagrangian
--------------------------------------------------------------------------------

data FU = FU

data AD2 = AD2

feasible = undefined
