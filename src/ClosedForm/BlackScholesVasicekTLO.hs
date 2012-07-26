{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClosedForm.BlackScholesVasicekTLO (
  blackScholesVasicekTLO
  ) where

import Statistics.Distribution
import Statistics.Distribution.Normal

import Math.GaussianQuadratureIntegration

import Types
import ClosedForm.VasicekZeroCouponBond


-- newtype IRVar = IRVar {unIRVar :: Double} deriving (Eq, Num, Show)
-- newtype StockVar = StockVar {unStockVar :: Double} deriving (Eq, Num, Show)
newtype CovarRS = CovarRS {unCovarRS :: Double} deriving (Eq, Num, Show)
newtype CorrelRS = CorrelRS {unCorrelRS :: Double} deriving (Eq, Num, Show)

psi :: BsvParams -> DiffTime -> Double 
psi (BsvParams ka' _ _ _ _) dt = (1- exp (-ka'*(toYears dt))) / ka'

expR :: BsvParams -> IR -> DiffTime -> IR
expR p@(BsvParams ka' th' sr' _ _) (IR r0) dt = IR $ r0 + ka'*(th'-r0)*psiL - 0.5*sr'*sr'*psiL*psiL
    where psiL = psi p dt

varR :: BsvParams -> DiffTime -> IRStd
varR (BsvParams ka' _ sr' _ _) dt = IRStd $ sqrt $ ((sr'*sr')/(2*ka'))*(1-exp (-2*ka'*(toYears dt)))
        
expS :: BsvParams -> IR -> Stock -> DiffTime -> Stock
expS p@(BsvParams ka' th' sr' ss' ro') (IR r0) (Stock s) dt =
  Stock $ log s + (term - 0.5*ss'*ss')*(toYears dt) - (term - r0)*psiL + ((sr'*sr')/(2*ka'))*psiL*psiL
    where
      term = th' - sr'*sr'/(ka'*ka') - ro'*ss'*sr'/ka'      
      psiL = psi p dt
      
varS :: BsvParams -> DiffTime -> StockStd
varS p@(BsvParams ka' _ sr' ss' ro') dt = 
  StockStd $ sqrt $ (sr'*sr'/(ka'*ka') + ss'*ss' + 2*ro'*sr'*ss'/ka')*dt' - (sr'*sr'/(ka'*ka')+2*ro'*sr'*ss'/ka')*psiL - sr'*sr'/(2*ka')*psiL*psiL
    where 
      dt'  = toYears dt
      psiL = psi p dt

covarRS :: BsvParams -> DiffTime -> CovarRS
covarRS p@(BsvParams _ _ sr' ss' ro') dt = CovarRS $ ro'*ss'*sr'*psiL + 0.5*sr'*sr'*psiL*psiL
  where psiL = psi p dt

correlRS :: CovarRS -> IRStd -> StockStd -> CorrelRS
correlRS (CovarRS covRS) (IRStd vr) (StockStd vs) = CorrelRS $ covRS / (vr*vs)
  
stdNormalCdf :: Double -> Double
stdNormalCdf x = cumulative d x
  where d = normalDistr 0.0 1.0
        
-- there was an error in this!        
stdNormalPdf :: Double -> Double
stdNormalPdf x = density d x
  where d = normalDistr 0 1
--stdNormalPdf x = (exp $ -0.5*x*x) / sqrt (2*pi)
        
integral :: Double -> Double -> Double -> Double -> Double
integral scoreR scoreS corrRS alpha = nIntegrate128 integrand (-9) upperBound
  where integrand = \u -> let y = (scoreS - alpha - corrRS*u) / sqrt (1 - corrRS*corrRS)
                          in u * stdNormalCdf y * stdNormalPdf u
        upperBound = scoreR - alpha*corrRS
        
-- import from VasicekZeroCouponBond.hs
-- zcbPrice :: BsvParams -> IR -> DiffTime -> Price
-- zcbPrice p@(BsvParams ka' th' sr' _ _) (IR r0) (DiffTime dt) = 
--   Price $ exp ((th - sr*sr/(2*k*k))*(psiL - dt) - sr*sr/(4*k)*psiL*psiL - psiL*r0)
--     where psiL = psi m (DiffTime dt)
--           k    = kappa m
--           th   = theta m
--           sr   = sigmaR m
        
-- Not great, but for now(?) Genz 2004: Numerical Computation of Rectangular Bivariate and Trivariate 
-- Normal and t Probabilities -- Equation 3
bivarStdNormalCdf :: Double -> Double -> Double -> Double
bivarStdNormalCdf x y rho = 
  stdNormalCdf (-h) * stdNormalCdf (-k) + 
  nIntegrate128 (\u -> let c = cos u in exp (-(h*h+k*k - 2*h*k*sin u)/(2*c*c)) / (2*pi)) 0 (asin rho)
    where h = -x
          k = -y
  
rScore :: IR -> IR -> IRStd -> Double
rScore (IR r) (IR mr) (IRStd vr) = (r-mr)/vr

sScore :: Stock -> Stock -> StockStd -> Double
sScore (Stock s) (Stock ms) (StockStd vs) = (log s-ms)/vs

-- Probably need to correct this, but could get it done today ;)
blackScholesVasicekTLO :: BsvParams -> IrStrike -> StockStrike -> IR -> Stock -> DiffTime -> Price
blackScholesVasicekTLO m rK' sK' r0 s0 dt = 
  (zcbPrice m r0 dt) * (Price $
  ((unIR (rK - mr))*(unStock sK)*bivarStdNormalCdf scoreR scoreS corrRS
   + (unIR (mr - rK) + (unCovarRS covRS)) * e2S * bivarStdNormalCdf (scoreR - corrRS*(unStockStd vs)) (scoreS - (unStockStd vs)) corrRS
   + (unIRStd vr)*(e2S*ivs - (unStock sK)*i0)))
    where rK     = IR (unIrStrike rK')
          sK     = Stock (unStockStrike sK')
          mr     = expR m r0 dt
          ms     = expS m r0 s0 dt
          vr     = varR m dt
          vs     = varS m dt
          e2S    = exp (unStock ms + 0.5*unStockStd vs*unStockStd vs)
          scoreR = rScore rK mr vr
          scoreS = sScore sK ms vs
          covRS  = covarRS m dt
          corrRS = unCorrelRS $ correlRS covRS vr vs 
          ivs    = integral scoreR scoreS corrRS (unStockStd vs)
          i0     = integral scoreR scoreS corrRS 0
        
-- bsvModel = BsvParams {kappa = 0.25, theta = 0.04, sigmaR = 0.02, sigmaS = 0.2, rho = 0.00 }

-- newtype Mpor = Mpor Double deriving (Eq, Num, Show)

-- toRiskNeutral :: BsvParams -> Mpor -> BsvParams
-- toRiskNeutral m (Mpor l) = BsvParams (kappa m) newTheta (sigmaR m) (sigmaS m) (rho m)
--   where newTheta = (theta m) - (sigmaR m)*l / (kappa m)
        
-- rnBsvModel = toRiskNeutral bsvModel (Mpor (-0.1))
          
-- main = putStrLn $ show $ 
--        tloPrice rnBsvModel (IR 0.03) (Stock 100) (IR 0.03) (Stock 100) (DiffTime 1.0)