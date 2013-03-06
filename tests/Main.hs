module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.AD.Lagrangian.Internal
import Control.Applicative

main = defaultMain [
        testGroup "trival test" [
            testCase "noConstraints" noConstraints,
            testCase "entropyTest" entropyTest
        ]
    ] 
    
    
noConstraints = (fst <$> actual) @?= Right expected where
    actual    = solve 0.00001 (f, []) 1
    expected  = [1]
    f [x] = -(x - 1) ^2
    
--class Approximate a where
--    x =~= y :: a -> a -> Bool


entropyTest = (sum . map abs $ zipWith (-) actual expected) < 0.02 @?= True  where
    Right actual = fst <$> solve 0.00001 (f, [(\xs -> sum xs, 1.0)]) 3
    expected  = [0.33, 0.33, 0.33]
    f :: Floating a => [a] -> a
    f = negate . sum . map (\x -> x * log x)
    
    

    
    