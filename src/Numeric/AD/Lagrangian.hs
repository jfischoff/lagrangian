-- |Numerically solve convex lagrange multiplier problems with conjugate gradient descent. 
--  
--  For example, find the maximum entropy with the constraint that the probabilities add
--  up to one. 
--  
--  
--  >>> solve 0.00001 (negate . sum . map (\x -> x * log x)) [sum <=> 1] 3
--  ([0.33, 0.33, 0.33], [-0.09])
--  
--  The first elements of the result pair are the arguments for the objective function at the minimum. 
--  The second elements are the lagrange multipliers.
module Numeric.AD.Lagrangian (
    -- *** Helper types
    AD2,
    (<=>),
    Constraint,
    -- ** Solver
    solve,
    -- *** Experimental features
    feasible) where
import Numeric.AD.Lagrangian.Internal (AD2, (<=>), solve, feasible, Constraint)