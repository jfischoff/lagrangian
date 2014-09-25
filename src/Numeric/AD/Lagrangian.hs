-- | Numerically solve convex Lagrange-multiplier problems with conjugate
--   gradient descent. 
--  
--   Consider an example from the Wikipedia page on Lagrange multipliers in
--   which we want to maximize the function f(x, y) = x + y, subject to the
--   constraint x^2 + y^2 = 1:
--   
--   >>> maximize (\[x, y] -> x + y) [(\[x, y] -> x^2 + y^2) <=> 1] 0.00001 2
--   Right ([0.707,0.707], [-0.707])
--   
--   The 'Right' indicates success; the first element of the pair is the
--   argument of the objective function at the maximum, and the second element
--   is a list of Lagrange multipliers.

module Numeric.AD.Lagrangian (
    -- *** Constraint type
    Constraint,
    (<=>),
    -- ** Optimizers
    maximize,
    minimize,
    -- *** Experimental features
    feasible) where

import Numeric.AD.Lagrangian.Internal
    ( Constraint
    , (<=>)
    , maximize
    , minimize
    , feasible
    )
