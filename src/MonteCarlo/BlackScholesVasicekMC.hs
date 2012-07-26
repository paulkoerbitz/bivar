{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
module MonteCarlo.BlackScholesVasicekMC (
  MVal(..), 
  PathChar(..),
  Dynamics(..),
  McResult(..),
  Path(..),
  genSamples,
  samplesToPath,
  makeBsvDynamics,
  makeBsvEulerDynamics,
  makeEulerDynamics,
  computeMcResult
  ) where

import System.Random hiding (next)
import Data.List
import Data.List.Split (chunk)

import qualified Statistics.Distribution.Normal as N
import qualified Statistics.Distribution as D

import Types
import Lattice

data MVal = MVal {vals :: BivarVal, intR :: Double}
newtype Path = Path {unPath :: [MVal]}
newtype Sample = Sample BivarVal deriving (Eq, Ord, Show)

data McResult = McResult {expVal :: Double, stdErr :: Double} deriving (Show)

data Dynamics = Dynamics {
  next :: MVal -> DiffTime -> Sample -> MVal
  }
                
data PathChar = PathChar {
  n :: Int,
  dt :: DiffTime,
  startVals :: MVal
  }

listToTuples :: [a] -> [(a,a)]
listToTuples []       = []
listToTuples (_:[])   = []
listToTuples (x:y:xs) = (x,y):listToTuples xs

genSamples :: Int -> [Sample]
genSamples n = map (Sample . BivarVal) (listToTuples $ map (D.quantile N.standard) (randoms $ mkStdGen n))

samplesToPath :: PathChar -> Dynamics -> [Sample] -> Path
samplesToPath (PathChar n dt sv) (Dynamics next) samples = 
  Path $ scanl (\old samp -> next old dt samp) sv (take n samples)

pricePath :: Option BivarVal DiffTime -> Path -> Price
pricePath o (Path ((MVal vals intR):[])) = (Price (exp (-intR)))*(payoff o vals)
pricePath o (Path ((MVal vals intR):xs)) = (Price (exp (-intR)))*(transition o vals 0.0 $ pricePath o (Path xs))

mcResult :: Int -> [Price] -> McResult
mcResult n ps = let (s, ss) = foldl' (\(!sum,!sumSq) (Price !p) -> (sum+p,sumSq+p*p)) (0,0) (take n ps)
                    avg     = s / fromIntegral n
                    var     = ss / fromIntegral n - avg*avg
                in McResult {expVal = avg, stdErr = sqrt (var / fromIntegral n)}
                   
makeBsvDynamics :: BsvParams -> Dynamics
makeBsvDynamics (BsvParams ka th sr ss ro) = Dynamics {
  next = \(MVal (BivarVal (r0,s0)) intR) dt' (Sample (BivarVal (wx,wy))) ->
   let dt      = toYears dt'
       ekdt    = exp (-ka*dt)
       psi     = (1-ekdt)/ka
       psi2    = (1-ekdt*ekdt)/(2*ka)
       expR    = r0*ekdt + th*(1-ekdt)
       stdR    = sr * sqrt psi2
       epsR    = stdR * wx
       r1      = expR + epsR
       expIntr = th*dt + (r0-th)*psi 
       epsIntr = sr/ka * sqrt (dt - 2*psi + psi2) * wx
       intr    = expIntr + epsIntr
       expY    = log s0 + (th - 0.5*ss*ss)*dt + (r0-th)*psi
       stdY    = sqrt $ ss*ss*dt + (2*ro*ss*sr/ka)*(dt-psi) + sr*sr/(ka*ka)*(dt - 2*psi + psi2)
       cov     = ro*ss*sr*psi + sr*sr/ka*(psi - psi2)
       corr    = cov / (stdR*stdY)
       wy'     = corr*wx + sqrt (1 - corr*corr)*wy
       epsY    = stdY * wy'
       s1      = exp $ expY + epsY
   in MVal {vals = BivarVal (r1,s1), intR = intr}
  }
                                             
makeBsvEulerDynamics :: BsvParams -> Dynamics
makeBsvEulerDynamics (BsvParams ka' th' sr' ss' ro') = Dynamics {
  next = \(MVal (BivarVal (r0,s0)) intR) dt' (Sample (BivarVal (wx,wy))) ->
   let dt''   = toYears dt'
       sdt  = sqrt dt''
       r1   = r0 + ka'*(th'-r0)*dt'' + sr'*sdt*wx
       wy'  = ro'*wx + sqrt (1-ro'^2) * wy
       s1   = s0*exp ((r0 - 0.5*ss'^2)*dt'' + ss'*sdt*wy')
       intr = r0*dt''
   in MVal {vals = BivarVal (r1,s1), intR = intr}
  }

computeMcResult :: PathChar -> Dynamics -> Option BivarVal DiffTime -> Int -> McResult
computeMcResult pc dyn o nSamples = 
  mcResult nSamples $ map ((pricePath o) . (samplesToPath pc dyn)) (chunk (n pc) $ genSamples 0)


makeEulerDynamics :: Model BivarVal HBivarVal DiffTime -> Dynamics
makeEulerDynamics m = Dynamics {
  next = \(MVal rs0 _) dt' (Sample (BivarVal (wx,wy))) ->
   let xy0    = toHomoskedastic m rs0 
       eps    = HBivarVal (wx, rho*wx + sqrt (1-rho^2)*wy)
       drft   = drift proc xy0
       vola   = volatility proc xy0
       expVal = expectation eulerDiscr xy0 drft dt'
       std    = stddev eulerDiscr vola dt'
       xy1    = expVal + std*eps
       rs1    = fromHomoskedastic m xy1
   in MVal {vals = rs1, intR = (negate . log . unPrice) (discount m rs0 dt')}
  }
  where 
    proc   = process m       
    rho    = correlation proc