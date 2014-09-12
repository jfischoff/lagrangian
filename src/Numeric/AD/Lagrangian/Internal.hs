{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Numeric.AD.Lagrangian.Internal where

import Numeric.AD
import Numeric.AD.Internal.Reverse

import Numeric.Optimization.Algorithms.HagerZhang05
-- import Numeric.LinearAlgebra.Algorithms

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Packed.Vector as V
-- import qualified Data.Packed.Matrix as M

import Data.Reflection

import GHC.IO (unsafePerformIO)

--------------------------------------------------------------------------------
-- The goal is to abstract `minimize` over `toMin` and `constraints`. ...
--------------------------------------------------------------------------------

minimize :: Double
         -> Int
         -> Either (Result, Statistics) (V.Vector Double, V.Vector Double)
minimize tolerance {-toMin constraints-} argCount = result where
    -- The function to minimize for the langrangian is the squared gradient
    obj argsAndLams =
        squaredGrad (lagrangian toMin constraints argCount) argsAndLams

    constraintCount = length constraints

    -- perhaps this should be exposed
    guess = U.replicate (argCount + constraintCount) (1.0 :: Double)

    --result = undefined
    result = case unsafePerformIO $
                    optimize
                        (defaultParameters {
                            printFinal = False
                          , verbose = VeryVerbose  -- For debugging ...
                          })
                        tolerance
                        guess
                        (VFunction (obj . U.toList))
                        (VGradient (U.fromList . grad obj . U.toList))
                        Nothing of
       (vs, ToleranceStatisfied, _) -> Right (S.take argCount vs,
                                              S.drop argCount vs)
       (_, x, y) -> Left (x, y)


lagrangian :: (Num a) => ([a] -> a) -> [([a] -> a, a)] -> Int -> [a] -> a
lagrangian f constraints argCount argsAndLams = result where
    args = take argCount argsAndLams
    lams = drop argCount argsAndLams

    -- g(x, y, ...) = c <=> g(x, y, ...) - c = 0
    appliedConstraints = fmap (\(g, c) -> g args - (c)) constraints

    -- L(x, y, ..., lam0, ...) = f(x, y, ...) + lam0 * (g0 - c0) ...
    result = (f args) + (sum . zipWith (*) lams $ appliedConstraints)


squaredGrad :: (Num a)
            => (forall s. (Reifies s Tape) => [Reverse s a] -> Reverse s a)
            -> [a] -> a
squaredGrad f = sum . fmap square . grad f where
    square x = x * x

--------------------------------------------------------------------------------
-- Run, e.g., `minimize (1e-10) 4` to verify that it works when closing over the
-- example objective function and constraints. 
--------------------------------------------------------------------------------

-- Right now, `minimize` closes over this ...
toMin :: (Floating a) => [a] -> a
toMin = entropy

-- ... and this.
constraints :: (Num a, Num b) => [([a] -> a, b)]
constraints = [probSum]

entropy :: (Floating a) => [a] -> a
entropy = sum . fmap (\x -> x * log x)

probSum :: (Num a, Num b) => ([a] -> a, b)
probSum = (sum, 1)

--------------------------------------------------------------------------------
-- Temporary stubs for code imported by Numeric.AD.Lagrangian
--------------------------------------------------------------------------------

data Constraint = Constraint

data FU = FU

data AD2 = AD2

(<=>) :: a
(<=>) = undefined

maximize :: a
maximize = undefined

feasible :: a
feasible = undefined
