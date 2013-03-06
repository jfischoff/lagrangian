{-# LANGUAGE Rank2Types #-}
module Numeric.AD.Lagrangian.Internal where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Numeric.LinearAlgebra.Algorithms
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M

-- In general I am fighting against the lack of type inference rank two types.
-- Hopefully some of the explicit type signatures can be removed.


-- The type for the contraints.
-- Given a constraint g(x, y, ...) = c, we would represent it as (g, c).
type Constraint a = ([a] -> a, a)

-- | This is not a true feasibility test for the function. I am not sure exactly how to 
--   implement that. This just checks the feasiblility at point. If this ever returns 
--   false, 'solve' can fail.
feasible :: (forall a. Floating a => ([a] -> a, [Constraint a], [a]))
      -> Bool
feasible params = result where
    obj :: Floating a => [a] -> a
    obj argsAndLams = squaredGrad lang argsAndLams

    lang :: Floating a => (forall s. Mode s => [AD s a] -> AD s a)
    lang = lagrangian fAndGs (length point)
    
    fAndGs :: (forall a. Floating a => ([a] -> a, [Constraint a]))
    fAndGs = (\(x, y, _) -> (x, y)) params
    
    point :: Floating a => [a]
    point = (\(_, _, x) -> x) params
    
    h :: [[Double]]
    h = hessian obj point
    -- I want the hessian as a matrix
    hessianMatrix = M.fromLists h

    -- make sure all of the eigenvalues are positive
    result = all (>0) . V.toList . eigenvaluesSH $ hessianMatrix 

-- | This is the lagrangrain multiplier solver. It is assumed that the 
--   objective function and all of the constraints take in the 
--   same about of arguments.
solve :: Double
      -> (forall a. Floating a => ([a] -> a, [Constraint a])) -- ^ A pair of the function to minimize and the constraints
      -> Int -- ^ The arity of the objective function and the constraints.
      -> Either (Result, Statistics) ([Double], [Double]) -- ^ Either an explaination of why the gradient descent failed or a pair of the arguments at the minimum and the lagrange multipliers
solve tolerance params argCount = result where
    obj :: Floating a => [a] -> a
    obj argsAndLams = squaredGrad lang argsAndLams

    lang :: Floating a => (forall s. Mode s => [AD s a] -> AD s a)
    lang = lagrangian params argCount
    
    constraintCount = length (snd params)
    
    guess = U.fromList $ replicate (argCount + constraintCount) (1.0 :: Double) 

    result = case unsafePerformIO (optimize (defaultParameters { printFinal = False }) 
                    tolerance guess (toFunction obj) (toGradient obj)
                       Nothing) of
        
       (vs, ToleranceStatisfied, _) -> Right (take argCount . S.toList $ vs, 
                                              drop argCount . S.toList $ vs) 
       (_, x, y) -> Left (x, y)

-- Convert a objective function and a list of constraints to a lagrangian
lagrangian :: Floating a
             => ([a] -> a, [Constraint a]) 
             -> Int
             -> [a] 
             -> a
lagrangian (f, constraints) argsLength argsAndLams = result where
    -- L(x, y, ..., lam0, lam1, ...) = f(x, y, ...) + 
    result = f args + (sum $ zipWith (*) lams appliedConstraints)
    
    -- Apply the arguments to the constraint function
    -- and subtract to set equal to zero
    -- (g, c) <=> g(x, y, ...) = c <=> g(x, y, ...) - c = 0
    appliedConstraints = map (\(f, c) -> f args - c) constraints

    -- Split the input by args and lambdas.
    -- It is assumed that the args for f and g's come before the
    -- lambdas for the constraints
    args = take argsLength argsAndLams
    lams = drop argsLength argsAndLams

sumMap f = sum . map f 

squaredGrad :: Num a 
            => (forall s. Mode s => [AD s a] -> AD s a) -> [a] -> a
squaredGrad f vs = sumMap (\x -> x*x) (grad f vs)

toFunction :: (forall a. Floating a => [a] -> a) -> Function Simple
toFunction f = VFunction (f . U.toList)

toGradient :: (forall a. Floating a => [a] -> a) -> Gradient Simple
toGradient f = VGradient (U.fromList . grad f . U.toList)

