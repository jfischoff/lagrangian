-- |Numerically solve convex lagrange multiplier problems with conjugate gradient descent. 
-- 
--  Convexity is key, otherwise the descent algorithm can return the wrong answer.
--  
--  Convexity can be tested by assuring that the hessian of the lagrangian is positive
--  definite over region the function is defined in. 
--  
--  I have provided test that the hessian is positive definite at a point, which is something,
--  but not enough to ensure that the whole function is convex.
--  
--  Be that as it may, if you know what the your lagrangian is convex you can use 'solve' to 
--  find the minimum.
--  
--  For example, find the maximum entropy with the constraint that the probabilities add
--  up to one. 
--  
--  @ 
--     solve 0.00001 (negate . sum . map (\x -> x * log x), [(sum, 1)]) 3
--  @
--  
--  Gives the answer ([0.33, 0.33, 0.33], [-0.09])
--  
--  The first elements of the result pair are the arguments for the objective function at the minimum. 
--  The second elements are the lagrange multipliers.
module Numeric.AD.Lagrangian (
    solve,
    Constraint) where
import Numeric.AD.Lagrangian.Internal (solve, feasible, Constraint)