module BivarTrinomialTree (
  closestNode,
  triBivarMoves,
  fastTBT,
  fastTBT',
  Opt2D
  ) where

import Types
import Lattice
import Memoization
-- import Debug.Trace

closestNode :: Grid BivarVal HBivarVal DiffTime Int BivarNode -> HBivarVal -> BivarNode
closestNode g (HBivarVal (x1,x2)) = BivarNode (quot1,quot2)
  where (HBivarVal (bx1,bx2)) = stepSize g
        (HBivarVal (sv1,sv2)) = startVals g
        sqrtDt = sqrt (toYears $ dt g)
        quot1 = round $ (x1-sv1) / (bx1 * sqrtDt)
        quot2 = round $ (x2-sv2) / (bx2 * sqrtDt)

data Prob1D = Prob1D {pd :: Double, pm :: Double, pu :: Double}

transProbs1D :: Double -> Double -> DiffTime -> Prob1D
transProbs1D b eps dt' = Prob1D { pd = p_d, pm = p_m, pu = p_u }
  where 
    dt     = toYears dt'
    sqrtDt = sqrt dt
    p_d = 1 / (2*b*b) + eps*eps / (2*b*b*dt) - eps / (2*b*sqrtDt)
    p_m = 1 - p_u - p_d
    p_u = 1 / (2*b*b) + eps*eps / (2*b*b*dt) + eps / (2*b*sqrtDt)

bivarNormalCondExpectation :: HBivarVal -> HBivarVal -> Double -> Double -> Double
bivarNormalCondExpectation (HBivarVal (muX,muY)) (HBivarVal (stdX,stdY)) rho x1 = 
  muY + stdY/stdX * rho * (x1-muX)
  

-- FIXME: Special cases to keep probs >= 0
triBivarMoves :: Grid BivarVal HBivarVal DiffTime Int BivarNode -> Discretization HBivarVal DiffTime -> 
                 Model BivarVal HBivarVal DiffTime -> BivarNode -> [(Weight, BivarNode)]
triBivarMoves g discr m n = [(Weight (df*x_pd*y_pdd), BivarNode (midX-1,midY-1)),
                             (Weight (df*x_pd*y_pdm), BivarNode (midX-1,midY  )),
                             (Weight (df*x_pd*y_pdu), BivarNode (midX-1,midY+1)),
                             (Weight (df*x_pm*y_pmd), BivarNode (midX  ,midY-1)),
                             (Weight (df*x_pm*y_pmm), BivarNode (midX  ,midY  )),
                             (Weight (df*x_pm*y_pmu), BivarNode (midX  ,midY+1)),
                             (Weight (df*x_pu*y_pud), BivarNode (midX+1,midY-1)),
                             (Weight (df*x_pu*y_pum), BivarNode (midX+1,midY  )),
                             (Weight (df*x_pu*y_puu), BivarNode (midX+1,midY+1))]
  where (Price df)                = (discount m) (nodeToVal g n) (dt g)
        curVal                    = fromGrid g n
        expVal                    = (expectation discr) curVal ((drift (process m)) curVal) (dt g)
        mid                       = closestNode g expVal
        (BivarNode (midX,midY))   = mid
        (HBivarVal (midXv,midYv)) = fromGrid g mid
        (HBivarVal (expX, _))     = expVal
        (HBivarVal (downXv, _))   = fromGrid g (BivarNode (midX-1,midY))
        (HBivarVal (upXv, _))     = fromGrid g (BivarNode (midX+1,midY))
        stdDevs                   = (stddev discr) (volatility (process m) curVal) (dt g)
        rho                       = correlation (process m)
        expY_given_X_down         = bivarNormalCondExpectation expVal stdDevs rho downXv
        expY_given_X_mid          = bivarNormalCondExpectation expVal stdDevs rho midXv
        expY_given_X_up           = bivarNormalCondExpectation expVal stdDevs rho upXv
        eps_X                     = expX - midXv
        eps_Y_given_X_down        = expY_given_X_down - midYv
        eps_Y_given_X_mid         = expY_given_X_mid  - midYv
        eps_Y_given_X_up          = expY_given_X_up   - midYv
        HBivarVal (bx,by)         = stepSize g
        Prob1D x_pd x_pm x_pu     = transProbs1D bx eps_X (dt g)
        stdAdjust                 = 1 / sqrt (1-rho^(2::Int))
        Prob1D y_pdd y_pdm y_pdu  = transProbs1D (by*stdAdjust) eps_Y_given_X_down (dt g)  -- Adjust by to reflect that
        Prob1D y_pmd y_pmm y_pmu  = transProbs1D (by*stdAdjust) eps_Y_given_X_mid  (dt g)  -- the conditional variance is
        Prob1D y_pud y_pum y_puu  = transProbs1D (by*stdAdjust) eps_Y_given_X_up   (dt g)  -- (1-rho^2)*original variance
        

 
type TBTLattice = Lattice BivarVal HBivarVal DiffTime Int BivarNode
type Model2D = Model BivarVal HBivarVal DiffTime
type Opt2D = Option BivarVal DiffTime

fastTBT :: TBTLattice -> Model2D -> Opt2D -> BivarNode -> Int -> Price
fastTBT l m o = memoize (lookupBivar . memoBivar) $ rollback l m o

fastTBT' :: TBTLattice -> Opt2D -> Price
fastTBT' l@(Lattice g _) o = Price (pricer 0 !. (BivarNode (0,0)))
  where 
    payoffPlane = makePayoffPlane l o
    finalIndx   = nTimeSteps g
    pricer      = \tIndx -> if tIndx == finalIndx 
                            then payoffPlane 
                            else rollback' l o tIndx (pricer (tIndx+1))
    
      
  