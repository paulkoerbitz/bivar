module Helpers (
  assertAbsDiffLeq
  ) where

import qualified Test.HUnit as HUnit

assertAbsDiffLeq :: Double -> Double -> Double -> HUnit.Assertion
assertAbsDiffLeq expected actual tol = HUnit.assertBool msg $ abs (expected - actual) < tol
  where 
    msg = "Expected |expected - actual| < " ++ show tol ++ " but got |"  ++ 
          show expected ++ " - " ++ show actual ++ "| = "++ show (abs (expected-actual))