module Main where

import Control.Applicative

import qualified Data.Vector.Storable as S

import Numeric.AD.Lagrangian.Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

--------------------------------------------------------------------------------
-- HUnit tests
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
 
-- This is now duplicative of the `uniformMaxEnt` property below.
entropyTest :: Assertion
entropyTest = (totalAbsDiff actual expected < 0.02) @?= True where
    Right actual = fst <$> maximize entropy [sum <=> 1] 0.00001 3
    expected  = S.fromList [0.33, 0.33, 0.33]

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "Unconstrained max-ent distribution is uniform" uniformMaxEnt
  ]

-- With only the normalization constraint, the uniform distribution is the
-- maximum-entropy distribution for a sample space of any finite cardinality.
uniformMaxEnt :: Int -> Bool
uniformMaxEnt n = totalAbsDiff actual expected < 0.01 where
    Right (actual, _) = maximize entropy [sum <=> 1] (1e-5) n
    expected = S.replicate n $ 1 / (fromIntegral n)

--------------------------------------------------------------------------------
-- Objective functions to test
--------------------------------------------------------------------------------

entropy :: (Floating a) => [a] -> a
entropy = negate . sum . fmap (\x -> x * log x)

-- A simple quadratic function of arity one
quadratic1 :: (Floating a) => [a] -> a
quadratic1 [x] = negate $ square (x - 1) where
    square x = x * x

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- Compute the sum of element-wise absolute differences of two vectors
totalAbsDiff :: (S.Storable a, Num a) => S.Vector a -> S.Vector a -> a
totalAbsDiff xs ys = S.sum . S.map abs $ S.zipWith (-) xs ys

--------------------------------------------------------------------------------
-- Unused. Finish or delete.
--------------------------------------------------------------------------------

--class Approximate a where
--    x =~= y :: a -> a -> Bool
