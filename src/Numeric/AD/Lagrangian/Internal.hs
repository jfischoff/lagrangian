{-# LANGUAGE Rank2Types, FlexibleContexts #-}
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
import Numeric.AD.Internal.Tower

infixr 1 <=>
-- | This is just a little bit of sugar for (,) to make constraints look like 
--  equals
(<=>) :: ([a] -> a) -> a -> Constraint a
g <=> c = (g,c)
-- | The type for the contraints.
--   Given a constraint @g(x, y, ...) = c@, we would represent it as @(g, c)@.
--   or with sugar @g@ '<=>' @c@
type Constraint a = ([a] -> a, a)

type AD2 s r a = AD s (AD r Double)

-- | This is the lagrangian multiplier solver. It is assumed that the 
--   objective function and all of the constraints take in the 
--   same amount of arguments.
minimize :: Double
      -> (forall s r. (Mode s, Mode r) => [AD2 s r Double] -> AD2 s r Double) 
        -- ^ The function to minimize
      -> (forall s r. (Mode s, Mode r) => [Constraint (AD2 s r Double)] ) 
      -- ^ The constraints as pairs @g \<=\> c@ which represent equations 
      --   of the form @g(x, y, ...) = c@
      -> Int 
      -- ^ The arity of the objective function which should equal the arity of 
      --   the constraints.
      -> Either (Result, Statistics) (S.Vector Double, S.Vector Double) 
      -- ^ Either an explanation of why the gradient descent failed or a pair 
      --   containing the arguments at the minimum and the lagrange multipliers
minimize tolerance toMin constraints argCount = result where
    -- The function to minimize for the langrangian is the squared gradient
    obj argsAndLams = 
        squaredGrad (lagrangian toMin constraints argCount) argsAndLams
    
    -- The mode does matter but I need to add annotation for the type checker
    constraintCount = length (constraints :: [Constraint (AD Tower (AD Tower Double))])
    
    -- perhaps this should be exposed
    guess = U.replicate (argCount + constraintCount) (1.0 :: Double) 
    
    result = case unsafePerformIO $ 
                    optimize 
                        (defaultParameters { printFinal = False }) 
                        tolerance 
                        guess 
                        (VFunction (lowerFU obj . U.toList)) 
                        (VGradient (U.fromList . grad obj . U.toList))
                        Nothing of
       (vs, ToleranceStatisfied, _) -> Right (S.take argCount vs, 
                                              S.drop argCount vs) 
       (_, x, y) -> Left (x, y)
       
-- | Finding the maximum is the same as the minimum with the objective function inverted
maximize :: Double
      -> (forall s r. (Mode s, Mode r) => [AD2 s r Double] -> AD2 s r Double) 
        -- ^ The function to maximize
      -> (forall s r. (Mode s, Mode r) => [Constraint (AD2 s r Double)] ) 
      -- ^ The constraints as pairs @g \<=\> c@ which represent equations 
      --   of the form @g(x, y, ...) = c@
      -> Int 
      -- ^ The arity of the objective function which should equal the arity of 
      --   the constraints.
      -> Either (Result, Statistics) (S.Vector Double, S.Vector Double) 
      -- ^ Either an explanation of why the gradient descent failed or a pair 
      --   containing the arguments at the minimum and the lagrange multipliers
maximize tolerance toMax constraints argCount = 
    minimize tolerance (negate1 . toMax) constraints argCount

lagrangian :: Num a 
           => ([a] -> a)
           -> [Constraint a]
           -> Int
           -> [a]  
           -> a
lagrangian f constraints argCount argsAndLams = result where
    args = take argCount argsAndLams
    lams = drop argCount argsAndLams
    
    -- g(x, y, ...) = c <=> g(x, y, ...) - c = 0
    appliedConstraints = fmap (\(f, c) -> f args - c) constraints
    
    -- L(x, y, ..., lam0, ...) = f(x, y, ...) + lam0 * (g0 - c0) ... 
    result = f args + (sum . zipWith (*) lams $ appliedConstraints)

squaredGrad :: Num a
            => (forall s. Mode s => [AD s a] -> AD s a) 
            -> [a] -> a
squaredGrad f vs = sum . fmap (\x -> x*x) . grad f $ vs

-- | WARNING. Experimental.
--   This is not a true feasibility test for the function. I am not sure 
--   exactly how to implement that. This just checks the feasiblility at a point.
--   If this ever returns false, 'solve' can fail.
feasible :: (forall s r. (Mode s, Mode r) => [AD2 s r Double] -> AD2 s r Double)
         -> (forall s r. (Mode s, Mode r) => [Constraint (AD2 s r Double)] )
         -> [Double]
         -> Bool
feasible toMin constraints points = result where
    obj argsAndLams = 
        squaredGrad (lagrangian toMin constraints $ length points) argsAndLams
    
    hessianMatrix = M.fromLists . hessian obj $ points
    
    -- make sure all of the eigenvalues are positive
    result = all (>0) . V.toList . eigenvaluesSH $ hessianMatrix


