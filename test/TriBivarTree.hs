module Main where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Options
import Types
import Lattice
import Helpers
import BivarTrinomialTree
import ClosedForm.VasicekZeroCouponBond
import ClosedForm.BlackScholesVasicekCall

main :: IO ()
main = defaultMain tests

instance Arbitrary BsvParams where
  arbitrary = do 
    kappa  <- choose (1.0e-6,10)
    theta  <- choose (0, 0.3)
    sigmaR <- choose (1.0e-6, 0.2)
    sigmaS <- choose (1.0e-6, 1)
    rho    <- choose (-1.0, 1.0)
    return (BsvParams {ka = kappa, th = theta, sr = sigmaR, ss = sigmaS, ro = rho})
    
instance Arbitrary BivarNode where
  arbitrary = do
    x <- choose(-100,100)---500, 500)
    y <- choose(-100,100)--(-500, 500)
    return (BivarNode (x,y))
    
tests :: [TF.Test]
tests = [ testGroup "Test triBivarMoves"
          [ testProperty "Probabilities sum to one" prop_probabilities_sum_to_one
          , testProperty "Probabilities are >= 0  " prop_probabilities_geq_zero
          , testProperty "unconditional expectations match" prop_unconditional_expectations_match
          -- , testProperty "moves do not move below minimum" undefined
          ]
        , testGroup "Option prices"
          [ 
          --   testProperty "Zero Coupon Bond     " prop_correct_zcb_price
          -- , 
          --   testProperty "Call Option, fixed IR" prop_correct_bsCall_fixed_ir
          -- , 
            testProperty "Call Option, free IR " prop_correct_bsCall_free_ir 
          ]
        ]
        
        
        
prop_probabilities_sum_to_one :: BsvParams -> BivarNode -> Property
prop_probabilities_sum_to_one bsvParams n = 
  property $ abs (sumWeights - 1) < 1e-6
  where
    m          = makeBsvModel bsvParams
    g          = makeGrid (toHomoskedastic m $ BivarVal (0.03, 100)) (HBivarVal (sqrt 3, sqrt 3)) 1.0 100
    nodes      = triBivarMoves g eulerDiscr m n
    sumWeights = foldl (\acc (Weight w, _) -> acc + w) 0 nodes

        
prop_probabilities_geq_zero :: BsvParams -> BivarNode -> Property
prop_probabilities_geq_zero bsvParams n = 
  property $ allGeqZero
  where
    m          = makeBsvModel bsvParams
    g          = makeGrid (toHomoskedastic m $ BivarVal (0.03, 100)) (HBivarVal (sqrt 3, sqrt 3)) 1.0 100
    nodes      = triBivarMoves g eulerDiscr m n
    allGeqZero = foldl (\acc (Weight w, _) -> acc && (w>=0)) True nodes

weightBvV :: Weight -> HBivarVal -> HBivarVal
weightBvV (Weight w) (HBivarVal (x,y)) = HBivarVal (w*x, w*y)

prop_unconditional_expectations_match :: BsvParams -> BivarNode -> Property
prop_unconditional_expectations_match bsvParams node =
  property $ abs (unconditionalExp - movesExp) < HBivarVal (1e-4,1e-4)
    where 
      node             = BivarNode (0,0)
      m                = makeBsvModel bsvParams
      g                = makeGrid (toHomoskedastic m $ BivarVal (0.03, 100)) (HBivarVal (sqrt 3, sqrt 3)) 1.0 100
      startVals        = fromGrid g node
      nodes            = triBivarMoves g eulerDiscr m node
      movesExp         = foldl (\acc (w, n') -> acc + weightBvV w (fromGrid g n')) (HBivarVal (0,0)) nodes
      unconditionalExp = (expectation eulerDiscr) startVals (drift (process m) startVals) (dt g)
      

prop_correct_zcb_price :: BsvParams -> Property
prop_correct_zcb_price bsvParams =
  let nSteps   = 50
      maturity = 5.0
      bsvModel = makeBsvModel bsvParams
      lat = Lattice {
        grid = makeGrid (toHomoskedastic bsvModel (BivarVal (0.03, 100))) (HBivarVal (sqrt 3, sqrt 3)) maturity nSteps,
        discr = eulerDiscr,
        mvs = Moves triBivarMoves
        }
      Price treePrice = fastTBT lat bsvModel zeroCouponBond (BivarNode (0,0)) nSteps
      Price cfPrice   = zcbPrice bsvParams (IR 0.03) (TimeDelta maturity)
  in property $ abs (treePrice - cfPrice) < 1e-2
                  
prop_correct_bsCall_fixed_ir :: BsvParams -> Property
prop_correct_bsCall_fixed_ir (BsvParams _ _ _ sS rho) =
  let nSteps   = 50
      maturity = 1.0 
      bsvParams = BsvParams {ka = 500, th = 0.03, sr = 0.00001, ss = sS, ro = rho}
      bsvModel = makeBsvModel bsvParams
      lat = Lattice {
        grid = makeGrid (toHomoskedastic bsvModel (BivarVal (0.03, 100))) (HBivarVal (sqrt 3, sqrt 3)) maturity nSteps,
        discr = eulerDiscr,
        mvs = Moves triBivarMoves
        }
      Price treePrice = fastTBT lat bsvModel euroCall (BivarNode (0,0)) nSteps
      Price cfPrice   = blackScholesVasicekCall bsvParams (Strike 100) (Stock 100) (IR 0.03) (TimeDelta maturity)
  in property $ abs (treePrice - cfPrice) < 1e-2
     
     
prop_correct_bsCall_free_ir :: BsvParams -> Property
prop_correct_bsCall_free_ir bsvParams =
  let nSteps   = 50
      maturity = 1.0 
      bsvModel = makeBsvModel bsvParams
      lat = Lattice {
        grid = makeGrid (toHomoskedastic bsvModel (BivarVal (0.03, 100))) (HBivarVal (sqrt 3, sqrt 3)) maturity nSteps,
        discr = eulerDiscr,
        mvs = Moves triBivarMoves
        }
      Price treePrice = fastTBT lat bsvModel euroCall (BivarNode (0,0)) nSteps
      Price cfPrice   = blackScholesVasicekCall bsvParams (Strike 100) (Stock 100) (IR 0.03) (TimeDelta maturity)
  in property $ abs (treePrice - cfPrice) < 1e-2