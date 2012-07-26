module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Types
import Helpers
import ClosedForm.BlackScholesVasicekCall

main :: IO ()
main = defaultMain tests

constructBsvTest :: String -> RabinovichTestCase -> TF.Test
constructBsvTest s tc = testCase s $ assertAbsDiffLeq exp act tol
  where (Price exp) = res tc
        (Price act) = blackScholesVasicekCall (p tc) (sK tc) (s0 tc) (r0 tc) (TimeDelta 1.0)
        tol         = 0.01

tests :: [TF.Test]
tests = [ testGroup "Known values Euro Call in Black-Scholes-Vasicek" 
          (map (\(n,tc) -> constructBsvTest ("RabinovichTestValue " ++ show n) tc)
           (zipWith (,) [1..] rabinovichTestValues))
        ]
            
               
            
  