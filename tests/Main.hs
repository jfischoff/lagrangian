module Main where

import Control.Applicative

import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Vector.Storable as S

import Numeric.AD.Lagrangian.Internal


main = defaultMain [
        testGroup "trival test" [
            testCase "noConstraints" noConstraints,
            testCase "entropyTest" entropyTest
        ]
    ] 
    
    
noConstraints = (fst <$> actual) @?= Right expected where
    actual    = minimize f [] 0.00001 1
    expected  = S.fromList [1]
    f [x] = -(x - 1) ^2
    
--class Approximate a where
--    x =~= y :: a -> a -> Bool

entropyTest = absDifference < 0.02 @?= True where
    absDifference = (S.sum . S.map abs $ S.zipWith (-) actual expected)
    Right actual = fst <$> maximize entropy [sum <=> 1] 0.00001 3
    expected  = S.fromList [0.33, 0.33, 0.33]
    
--------------------------------------------------------------------------------
-- Objective functions to test
--------------------------------------------------------------------------------

entropy :: (Floating a) => [a] -> a
entropy = negate . sum . fmap (\x -> x * log x)
