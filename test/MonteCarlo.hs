{-# LANGUAGE BangPatterns #-}
module Main where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test

--import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List.Split (chunk)

import Types
import Options
import Helpers
import MonteCarlo.BlackScholesVasicekMC
import ClosedForm.BlackScholesVasicekCall
import ClosedForm.VasicekZeroCouponBond

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
    

tests :: [TF.Test]
tests = [ testGroup "Test Monte Carlo results against closed form formulas" 
          [ testProperty "MC: Exact dynamics, zero coupon bond" prop_correct_zcb_price
          , testProperty "MC: Exact dynamics, euro call"        prop_correct_eurocall_price
          -- , testProperty "MC: Exact dynamics, euro TLO"         prop_correct_euroTLO_price
          ]
        , 
          testGroup "Test MC path distribution properties" 
          [ testProperty "Euler and exact dynamics give similar interest rate paths" prop_similar_ir_paths
          , testProperty "Euler and exact dynamics give similar stock paths" prop_similar_stock_paths
          , testProperty "Euler and exact dynamics give similar interest integral along paths" prop_similar_intR_paths
          , testProperty "Euler and exact dynamics give similar ir expected values" prop_similar_ir_means
          , testProperty "Euler and exact dynamics give similar stock expected values" prop_similar_stock_means
          , testProperty "Euler and exact dynamics give similar intR expected values" prop_similar_intR_means
          ]
        -- , testGroup "Test MC results European call option in Black-Scholes Vasicek model" 
        --   [ testProperty "Hello, world" undefined
        --   , -- things to test: euler / non-euler dynamics give approx same price as regular bs
        --   ]
        -- , testGroup "Test MC results against know values of the Traffic Light Option"
        --   [ testProperty "Hello, world" undefined
        --   , -- things to test: euler / non-euler dynamics give approx same price as regular bs
        --   ]
        ]
        
prop_similar_paths :: (MVal -> Double) -> Double -> BsvParams -> Int -> Property
prop_similar_paths f tol bsvParams seed =
  let pc             = PathChar {n = 10000, dt = TimeDelta 0.0001, startVals = MVal (BivarVal (0.03, 100)) 0.0}
      dynExact       = makeBsvDynamics bsvParams
      dynEuler       = makeBsvEulerDynamics bsvParams
      path1          = samplesToPath pc dynExact (genSamples seed)
      path2          = samplesToPath pc dynEuler (genSamples seed)
      pathSimilar    = \acc (x,y) -> acc && (abs (f x - f y) < tol)
  in property $ foldl pathSimilar True (zip (unPath path1) (unPath path2))

getStock :: MVal -> Double
getStock (MVal (BivarVal (_,s)) _) = s

getIr :: MVal -> Double
getIr (MVal (BivarVal (r,_)) _) = r

getIntR :: MVal -> Double
getIntR (MVal _ intr) = intr

prop_similar_ir_paths :: BsvParams -> Int -> Property
prop_similar_ir_paths = prop_similar_paths getIr 1e-3

prop_similar_stock_paths :: BsvParams -> Int -> Property
prop_similar_stock_paths = prop_similar_paths getStock 1e-1

prop_similar_intR_paths :: BsvParams -> Int -> Property
prop_similar_intR_paths = prop_similar_paths getIntR 1e-4

average :: Fractional a => [a] -> a
average xs = res_sum / fromIntegral res_len
  where (res_len, res_sum) = foldl (\(!len,!acc) x -> (len+1,acc+x)) (0 :: Int,0) xs

nPaths_similar :: Int -> ([Path] -> [Path] -> Bool) -> BsvParams -> Int -> Property
nPaths_similar nPaths comparer bsvParams seed = 
  let pcExact    = PathChar {n=1, dt = TimeDelta 1, startVals = MVal (BivarVal (0.03, 100)) 0.0}
      pcEuler    = PathChar {n=100, dt = TimeDelta 0.01, startVals = MVal (BivarVal (0.03, 100)) 0.0}
      dynExact   = makeBsvDynamics bsvParams
      dynEuler   = makeBsvEulerDynamics bsvParams
      pathsExact = map (samplesToPath pcExact dynExact) (chunk (n pcExact) $ genSamples seed)
      pathsEuler = map (samplesToPath pcEuler dynEuler) (chunk (n pcEuler) $ genSamples seed)
      in property $ comparer (take nPaths pathsExact) (take nPaths pathsEuler)

avgAndSe :: Floating a => [a] -> (a,a)
avgAndSe xs = (avg, se)
  where (s, ssq, len) = foldl (\(!s', !ssq', !len') x -> (s'+x, ssq'+x*x, len'+1)) (0,0,0) xs
        avg          = s / fromInteger len
        se           = sqrt (ssq / fromInteger len - avg*avg)

compare_mc_res :: (Path -> Double) -> [Path] -> [Path] -> Bool
compare_mc_res f p1 p2 = lb1 <= mu2 && mu2 <= ub1 && lb2 <= mu1 && mu1 <= ub2
  where (mu1,se1) = avgAndSe (map f p1)
        (mu2,se2) = avgAndSe (map f p2)
        (lb1,ub1) = (mu1-4*se1, mu1+4*se1)
        (lb2,ub2) = (mu2-4*se2, mu2+4*se2)

prop_similar_ir_means :: BsvParams -> Int -> Property
prop_similar_ir_means = nPaths_similar 10000 (compare_mc_res (\(Path xs) -> getIr . last $ xs))

prop_similar_stock_means :: BsvParams -> Int -> Property
prop_similar_stock_means = nPaths_similar 10000 (compare_mc_res (\(Path xs) -> getStock . last $ xs))

prop_similar_intR_means :: BsvParams -> Int -> Property
prop_similar_intR_means = nPaths_similar 10000 (compare_mc_res (\(Path xs) -> sum (map getIntR xs)))

prop_correct_zcb_price :: BsvParams -> Property
prop_correct_zcb_price bsvParams =
  let pc             = PathChar {n = 1, dt = TimeDelta 5, startVals = MVal (BivarVal (0.03, 100)) 0.0}
      McResult ev se = computeMcResult pc (makeBsvDynamics bsvParams) zeroCouponBond 100000
      cfPrice        = zcbPrice bsvParams (IR 0.03) (TimeDelta (fromIntegral (n pc)*(unTimeDelta (dt pc))))
  in property $ abs (ev - unPrice cfPrice) < 5.5*se

prop_correct_eurocall_price :: BsvParams -> Property
prop_correct_eurocall_price bsvParams =
  let pc             = PathChar {n = 1, dt = TimeDelta 1, startVals = MVal (BivarVal (0.03, 100)) 0.0}
      McResult ev se = computeMcResult pc (makeBsvEulerDynamics bsvParams) euroCall 10000
      cfPrice        = blackScholesVasicekCall bsvParams (Strike 100) (Stock 100) (IR 0.03) (TimeDelta (fromIntegral (n pc)*(unTimeDelta (dt pc))))
  in property $ abs (ev - unPrice cfPrice) < 5*se
      
-- prop_correct_euroTLO_price :: BsvParams -> Property
-- prop_correct_euroTLO_price bsvParams = property $ True
--   let pc             = PathChar {n = 1, dt = TimeDelta 1, startVals = MVal (BivarVal (0.03, 100)) 0.0}
--       McResult ev se = computeMcResult pc (makeBsvEulerDynamics bsvParams) euroCall 10000
--       cfPrice        = blackScholesVasicekCall bsvParams (Strike 100) (Stock 100) (IR 0.03) (TimeDelta (fromIntegral (n pc)*(unTimeDelta (dt pc))))
--   in property $ abs (ev - unPrice cfPrice) < 5*se
      