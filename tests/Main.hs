module Main where

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Vector.Storable as S

import Numeric.AD.Lagrangian.Internal


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "No constraints" noConstraints
  , testCase "Entropy test" entropyTest
  ]
    
noConstraints :: Assertion
noConstraints = (fst <$> actual) @?= Right expected where
    actual    = minimize quadratic1 [] 0.00001 1
    expected  = S.fromList [1]
    
entropyTest :: Assertion
entropyTest = absDifference < 0.02 @?= True where
    absDifference = (S.sum . S.map abs $ S.zipWith (-) actual expected)
    Right actual = fst <$> maximize entropy [sum <=> 1] 0.00001 3
    expected  = S.fromList [0.33, 0.33, 0.33]
    
--------------------------------------------------------------------------------
-- Objective functions to test
--------------------------------------------------------------------------------

entropy :: (Floating a) => [a] -> a
entropy = negate . sum . fmap (\x -> x * log x)

-- A basic quadratic function of arity one
quadratic1 :: (Floating a) => [a] -> a
quadratic1 [x] = negate $ square (x - 1) where
    square x = x * x

--------------------------------------------------------------------------------
-- Unused. Finish or delete.
--------------------------------------------------------------------------------

--class Approximate a where
--    x =~= y :: a -> a -> Bool
