-- |Numerically solve convex lagrange multiplier problems with conjugate gradient descent. 
--  
--  Here is an example from the Wikipedia page on Lagrange multipliers.
--  Maximize f(x, y) = x + y, subject to the constraint x^2 + y^2 = 1 
--  
--  >>> maximize 0.00001 (\[x, y] -> x + y) [(\[x, y] -> x^2 + y^2) <=> 1] 2
--  Right ([0.707,0.707], [-0.707])
--  
--  The first elements of the result pair are the arguments for the objective function at the minimum. 
--  The second elements are the lagrange multipliers.
module Numeric.AD.Lagrangian (
    -- *** Helper types
    AD2,
    (<=>),
    Constraint,
    -- ** Solver
    maximize,
    minimize,
    -- *** Experimental features
    feasible) where
import Numeric.AD.Lagrangian.Internal (AD2, (<=>), maximize, minimize, feasible, Constraint)