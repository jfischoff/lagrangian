{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Numeric.AD.Lagrangian.Internal where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M

import GHC.IO (unsafePerformIO)

import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05
import Numeric.LinearAlgebra.Algorithms

-- | An equality constraint of the form @g(x, y, ...) = c@. Use '<=>' to
-- construct a 'Constraint'.
newtype Constraint = Constraint
  {unConstraint :: forall a. (Floating a) => ([a] -> a, a)}

infixr 1 <=>
-- | Build a 'Constraint' from a function and a constant
(<=>) :: (forall a. (Floating a) => [a] -> a)
      -> (forall b. (Floating b) => b)
      -> Constraint
f <=> c = Constraint (f, c)

-- | Numerically minimize the Langrangian. The objective function and each of
-- the constraints must take the same number of arguments.
minimize :: (forall a t. Floating a => [a] -> a)
         -- ^ The objective function to minimize
         -> [Constraint]
         -- ^ A list of constraints @g \<=\> c@ corresponding to equations of
         -- the form @g(x, y, ...) = c@
         -> Double
         -- ^ Stop iterating when the largest component of the gradient is
         -- smaller than this value
         -> Int
         -- ^ The arity of the objective function, which must equal the arity of
         -- the constraints
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
         -- ^ Either a 'Right' containing the argmin and the Lagrange
         -- multipliers, or a 'Left' containing an explanation of why the
         -- gradient descent failed
minimize f constraints tolerance argCount = result where
    -- At a constrained minimum of `f`, the gradient of the Lagrangian must be
    -- zero. So we square the Lagrangian's gradient (making it non-negative) and
    -- minimize it.
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

-- | Numerically maximize the Langrangian. The objective function and each of
-- the constraints must take the same number of arguments.
maximize :: (forall a. Floating a => [a] -> a)
         -- ^ The objective function to minimize
         -> [Constraint]
         -- ^ A list of constraints @g \<=\> c@ corresponding to equations of
         -- the form @g(x, y, ...) = c@
         -> Double
         -- ^ Stop iterating when the largest component of the gradient is
         -- smaller than this value
         -> Int
         -- ^ The arity of the objective function, which must equal the arity of
         -- the constraints
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
         -- ^ Either a 'Right' containing the argmax and the Lagrange
         -- multipliers, or a 'Left' containing an explanation of why the
         -- gradient ascent failed
maximize f = minimize $ negate . f

lagrangian :: (Floating a)
           => (forall b. Floating b => [b] -> b)
           -> [Constraint]
           -> Int
           -> [a]
           -> a
lagrangian f constraints argCount argsAndLams = result where
    args = take argCount argsAndLams
    lams = drop argCount argsAndLams

    -- g(x, y, ...) = c <=> g(x, y, ...) - c = 0
    appliedConstraints = fmap (\(Constraint (g, c)) -> g args - c) constraints

    -- L(x, y, ..., lam0, ...) = f(x, y, ...) + lam0 * (g0 - c0) ...
    result = (f args) + (sum . zipWith (*) lams $ appliedConstraints)

squaredGrad :: (Floating a)
            => (forall b. (Floating b) => [b] -> b)
            -> [a]
            -> a
squaredGrad f = sum . fmap square . grad f where
    square x = x * x

-- | WARNING: Experimental.
--   This is not a true feasibility test for the function. I am not sure
--   exactly how to implement that. This just checks the feasiblility at a
--   point. If this ever returns false, 'solve' can fail.
feasible :: (Floating a, Field a, M.Element a)
         => (forall b. (Floating b) => [b] -> b)
         ->[Constraint]
         -> [a]
         -> Bool
feasible f constraints points = result where
    sqGradLgn :: (Floating a) => [a] -> a
    sqGradLgn = squaredGrad $ lagrangian f constraints $ length points

    hessianMatrix = M.fromLists . hessian sqGradLgn $ points

    -- make sure all of the eigenvalues are positive
    result = all (>0) . V.toList . eigenvaluesSH $ hessianMatrix
