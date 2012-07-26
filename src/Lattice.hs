{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lattice (
  StochasticProcess(..),
  Discretization(..),
  Grid(..),
  Model(..),
  Price(..),
  Lattice(..),
  Option(..),
  Weight(..),
  Plane(..),
  CfModel(..),
  makeBsvModel,
  makeCevCklsModel,
  makeEitherModel,
  rollback,
  makeGrid,
  eulerDiscr,
  (!.),
  rollback',
  minNode,
  maxNode,
  makePayoffPlane,
  makeBsvCfModel
  ) where 

import qualified Data.Array.Repa as R
import Types

import Debug.Trace

data StochasticProcess h = StochasticProcess { 
  drift :: h -> h, 
  volatility :: h -> h,
  correlation :: Double,
  minVal :: h 
  }

data Discretization h t = Discretization {
  expectation :: h -> h -> t -> h, 
  stddev :: h -> t -> h
  }

data Grid v h t i n = Grid { 
  startVals :: h, 
  stepSize :: h, 
  dt :: t, 
  nTimeSteps :: i, 
  dx :: h, 
  fromGrid :: n -> h,
  getCurrentTime :: i -> t,
  nodeToVal :: n -> v
  }

data Model v h t = Model {
  process :: StochasticProcess h,
  discount :: v -> t -> Price,     -- this is not really a property of the model but the discretization
  toHomoskedastic :: v -> h,
  fromHomoskedastic :: h -> v
  }

data CfModel v h t = CfModel {
  model :: Model v h t,
  hDensity :: DiffTime -> h -> h -> Double,
  hDiscount :: DiffTime -> h -> h -> Double,
  hMean :: DiffTime -> h -> h,
  hStd :: DiffTime -> h,
  hCorr :: DiffTime -> Double
}

newtype Weight = Weight Double deriving (Show)

data Lattice v h t i n = Lattice {
  grid :: Grid v h t i n, 
  moves :: n -> [(Weight, n)]
  }

data Option v t = Option {
  maturity :: t,
  payoff :: v -> Price,
  transition :: v -> t -> Price -> Price
  }


makeBsvModel :: BsvParams -> Model2D
makeBsvModel (BsvParams k t sR sS rho) = Model {
  process = StochasticProcess {
     drift = \(HBivarVal (x,y)) -> HBivarVal (k*(t/sR - x), x*sR/sS - 0.5*sS),
     volatility = \v -> (HBivarVal (1,1)),
     correlation = rho,
     minVal = HBivarVal (-1/0, -1/0)
     },
  discount = \v dt -> let (BivarVal (r,_)) = v in Price $ exp (-r*(toYears dt)),
  toHomoskedastic = \(BivarVal (r,s)) -> HBivarVal (r / sR, log s / sS),
  fromHomoskedastic = fromHomoskedastic'
  }
  where fromHomoskedastic' = \(HBivarVal (x,y)) -> BivarVal (x*sR, exp (y*sS))
        

makeCevCklsModel :: CevCklsParams -> Model2D
makeCevCklsModel (CevCklsParams (BsvParams ka' th' sr' ss' ro') xi' al') = Model {
  process = StochasticProcess {
     drift = \(HBivarVal (x,y)) -> HBivarVal (r2x (ka'*(th' - x2r x)), s2y (y2s y*x2r x)),
     volatility = \v -> (HBivarVal (1,1)),
     correlation = ro',
     minVal = let minX | 0 < xi' && xi' < 1 = 0 
                       | otherwise          = -1/0
                  minY | 0 < al' && al' < 1 = 0
                       | otherwise          = -1/0
              in HBivarVal (minX,minY)
     },
  discount = \(BivarVal (r,_)) dt -> Price $ exp (-r*(toYears dt)),
  toHomoskedastic   = \(BivarVal (r,s)) -> HBivarVal (r2x r, s2y s),
  fromHomoskedastic = \(HBivarVal (x,y)) -> BivarVal (x2r x, y2s y)
  }
  where 
    toHom = \vol expo -> let f | expo == 0 = id
                               | expo == 1 = \h -> log h / vol
                               | otherwise = \h -> h**(1-expo) / (vol*(1-expo))
                         in f
    toHet = \vol expo -> let f | expo == 0 = id
                               | expo == 1 = \h -> exp (h*vol)
                               | otherwise = \h -> (h*vol*(1-expo))**(1/(1-expo))
                         in f
    r2x = toHom sr' xi'
    x2r = toHet sr' xi'
    s2y = toHom ss' al'
    y2s = toHet ss' al'
    
    
makeEitherModel :: Parameters -> Model2D
makeEitherModel (Left p)  = makeBsvModel p 
makeEitherModel (Right p) = makeCevCklsModel p
        

bsvHMean :: BsvParams -> DiffTime -> HBivarVal -> HBivarVal
bsvHMean (BsvParams ka' th' sr' ss' ro') dt (HBivarVal (x0,y0)) = HBivarVal (meanX,meanY) 
  where 
    dt'   = toYears dt
    meanX = x0*exp (-ka'*dt') + (th'/sr')*(1 - exp (-ka'*dt'))
    meanY = y0 + (th'/ss' - 0.5*ss')*dt' + (x0*sr'/ss' - th'/ss')*(1 - exp (-ka'*dt'))/ka'


bsvHStd :: BsvParams -> DiffTime -> HBivarVal
bsvHStd (BsvParams ka' th' sr' ss' ro') dt  = HBivarVal (stdX,stdY)
  where
    dt'   = toYears dt
    eKaDt = exp (-ka'*dt')
    psi   = (1 - eKaDt) / ka'
    psi2  = (1 - eKaDt^2) / (2*ka')
    stdX  = sqrt $ psi2
    stdY  = sqrt $ dt' + (2*ro'*sr'/(ka'*ss'))*(dt' - psi) + (sr'^2/(ka'^2*ss'^2))*(dt' - 2*psi + psi2)


bsvHCovar :: BsvParams -> DiffTime -> Double
bsvHCovar = bsvHCovar2
    
bsvHCovar1 :: BsvParams -> DiffTime -> Double
bsvHCovar1 p'@(BsvParams _ _ _ _ ro') dt' = fctr * stdX * (sqrt $ stdY^2 - (1-ro'^2)*dt'')
  where 
    fctr    = if ro' >= 0 then 1 else -1    
    dt''    = toYears dt'
    HBivarVal (stdX, stdY) = bsvHStd p' dt'
    
bsvHCovar2 :: BsvParams -> DiffTime -> Double
bsvHCovar2 (BsvParams ka' th' sr' ss' ro') dt' = (ro'*psi + (sr'/(ka'*ss'))*(psi-psi2)) 
  where 
    dt''    = toYears dt'
    eKaDt  = exp (-ka'*dt'')
    psi    = (1 - eKaDt) / ka'
    psi2   = (1 - eKaDt^2) / (2*ka')
          
    
bivarNormDens :: (Double,Double) -> (Double,Double) -> Double -> (Double,Double) -> Double
bivarNormDens (meanX,meanY) (stdX,stdY) cov (x,y) = 
  exp (-(((x-meanX)/stdX)^2 + ((y-meanY)/stdY)^2 - 2*rho*(x-meanX)*(y-meanY)/(stdY*stdX))/(2*(1-rho^2))) 
  / (2*pi*stdX*stdY*sqrt (1-rho^2))
    where 
      rho = cov/(stdX*stdY)


makeBsvCfModel :: BsvParams -> CfModel BivarVal HBivarVal DiffTime
makeBsvCfModel p@(BsvParams ka' th' sr' ss' ro') = CfModel {
  model    = model',
  hDensity = \dt' h0@(HBivarVal (x0,y0)) h1@(HBivarVal (x1,y1)) -> 
                let HBivarVal means = bsvHMean p dt' h0 
                    HBivarVal stds  = bsvHStd p dt'
                    cov             = bsvHCovar p dt'
                in bivarNormDens means stds cov (x1,y1),
  hDiscount = \dt' h0@(HBivarVal (x0,_)) h1@(HBivarVal (x1,_)) -> 
    let dt''    = toYears dt'
        muX     = x0*exp (-ka'*dt'') + th'/sr'*(1-exp (-ka'*dt''))
        stdX    = sqrt $ (1-exp (-2*ka'*dt''))/(2*ka')
        eps     = (x1 - muX) / stdX
        psi     = (1-exp (-ka'*dt''))/ka'
        muIntR  = th'*dt'' + (x0*sr' - th')*psi
        stdIntR = sr'/ka' * (sqrt $ dt'' - 2*psi + (1-exp (-2*ka'*dt''))/(2*ka'))
        intR    = muIntR + stdIntR*eps
        BivarVal (r0,_) = fromHomoskedastic model' h0
        BivarVal (r1,_) = fromHomoskedastic model' h1
        intR'   = dt''*0.5*(r0+r1)
    in exp (-intR),
  hMean = bsvHMean p,
  hStd  = bsvHStd p,
  hCorr = \dt' -> let (HBivarVal (stdX,stdY)) = bsvHStd p dt' in (bsvHCovar p dt') / (stdX*stdY)
  }
  where 
    model' = makeBsvModel p


weightPrice :: Weight -> Price -> Price
weightPrice (Weight w) (Price p) = Price $ w*p


rollback :: (Eq i, Num i) => Lattice v h t i n -> Model v h t -> Option v t -> (n -> i -> Price) -> n -> i -> Price
rollback (Lattice g   _) m o _ n 0     = (payoff o) $ nodeToVal g n
rollback (Lattice g mvs) m o f n tIndx = discountFactor * weightedSum
  where 
    weightedSum    = foldl (+) (Price 0) weightedPrices
    curVal         = nodeToVal g n
    curTime        = getCurrentTime g tIndx
    discountFactor = (discount m) curVal (dt g)
    trans          = \x -> (transition o) curVal curTime (f (snd x) (tIndx-1))
    weightedPrices = map (\x -> weightPrice (fst x) $ trans x) weightedNodes
    weightedNodes  = mvs n

data Plane = Plane {plValues :: R.Array R.U R.DIM2 Double,  plMidIndex :: BivarNode} deriving (Show)

(!.) :: Plane -> BivarNode -> Double
(!.) (Plane vals mid) idx = vals R.! (R.Z R.:. ix R.:. iy)
  where BivarNode (ix,iy) =  idx + mid

getIx :: BivarNode -> Int
getIx (BivarNode (x,_)) = x

getIy :: BivarNode -> Int
getIy (BivarNode (_,y)) = y

minNode :: Lattice BivarVal HBivarVal DiffTime Int BivarNode -> Int -> BivarNode
minNode lat@(Lattice g mvs) tIndx = findNode (BivarNode (0,0)) move tIndx
  where
    move   = \node -> let nodes             = map snd $ mvs node
                          BivarNode (nX,nY) = node
                          minNode           = BivarNode (foldl1 min (map getIx nodes), foldl1 min (map getIy nodes))
                      in (all (\ (BivarNode (x,y)) -> x >=nX && y >=nY ) nodes, minNode)


maxNode :: Lattice BivarVal HBivarVal DiffTime Int BivarNode -> Int -> BivarNode
maxNode lat@(Lattice g mvs) tIndx = findNode (BivarNode (0,0)) move tIndx
  where 
    move = \node -> let nodes               = map snd $ mvs node
                        BivarNode (nX,nY) = node
                        maxNode           = BivarNode (foldl1 max (map getIx nodes), foldl1 max (map getIy nodes))
                    in (all (\ (BivarNode (x,y)) -> x <= nX && y <= nY ) nodes, maxNode)

findNode :: BivarNode -> (BivarNode -> (Bool,BivarNode)) -> Int -> BivarNode
findNode start move maxIter = if done || (maxIter == 0) then start else findNode newNode move (maxIter-1)
  where 
    (done,newNode) = move start 



rollback' :: Lattice BivarVal HBivarVal DiffTime Int BivarNode -> Option BivarVal DiffTime -> Int -> Plane -> Plane
rollback' lat o tIndx pl = makePlane lat rb tIndx
  where
    rb = singleNodeRollback lat o tIndx pl

    
singleNodeRollback :: Lattice BivarVal HBivarVal DiffTime Int BivarNode -> Option BivarVal DiffTime -> Int -> Plane -> BivarNode -> Double
singleNodeRollback (Lattice g mvs) o tIndx pl n = 
  let curVal         = nodeToVal g n
      curTime        = getCurrentTime g tIndx
      trans          = \n' -> (transition o) curVal curTime (Price (pl !. n'))
      weightedNodes  = mvs n
      Price result   = foldl (\acc (w,n) -> acc + weightPrice w (trans n)) (Price 0) weightedNodes
  in result
     
makePayoffPlane :: Lattice BivarVal HBivarVal DiffTime Int BivarNode -> Option BivarVal DiffTime -> Plane
makePayoffPlane l@(Lattice g _) o = makePlane l indexToValue (nTimeSteps g)
  where
    indexToValue  = unPrice . (payoff o) . (nodeToVal g) 
    
makePlane :: TBTLattice -> (BivarNode -> Double) -> Int -> Plane    
makePlane lat f tIndx = Plane vls' mid'
  where
    BivarNode (lbX, lbY) = minNode lat tIndx
    BivarNode (ubX, ubY) = maxNode lat tIndx
    relIndices           = [BivarNode (ix,iy) | ix <- [lbX..ubX], iy <- [lbY..ubY]]
    mid'                 = BivarNode ((-lbX),(-lbY))
    resultList           = map f relIndices
    vls'                 = R.fromListUnboxed (R.Z R.:. ((-lbX)+ubX+1::Int) R.:. ((-lbY)+ubY+1::Int)) resultList

    
type TBTGrid = Grid BivarVal HBivarVal DiffTime Int BivarNode
type StochProc2D = StochasticProcess BivarVal
type Discr2D = Discretization BivarVal DiffTime
type TBTLattice = Lattice BivarVal HBivarVal DiffTime Int BivarNode
type Model2D = Model BivarVal HBivarVal DiffTime
type Opt2D = Option BivarVal DiffTime


makeGrid :: Model2D -> HBivarVal -> HBivarVal -> DiffTime -> Int -> TBTGrid
makeGrid m sv ss mat nts =
  Grid { startVals = sv,
         stepSize = ss,
         nTimeSteps = nts,
         dt = gridDt,
         dx = gridDx,
         fromGrid = fromGrid',
         getCurrentTime = \i -> (fromIntegral $ nts-i)*gridDt,
         nodeToVal = \n -> (fromHomoskedastic m) $ fromGrid' n
       }
  where (HBivarVal (bx, by)) = ss
        gridDt = mat / (fromIntegral nts)
        gridDx = HBivarVal (bx * sqrt (toYears gridDt), by * sqrt (toYears gridDt))
        fromGrid' = \(BivarNode (n1,n2)) -> sv + gridDx*(HBivarVal (fromIntegral n1, fromIntegral n2))
        
eulerDiscr :: Discretization HBivarVal DiffTime
eulerDiscr = Discretization {
  expectation = \x0 drift dt -> x0 + drift*(HBivarVal (toYears dt, toYears dt)),
  stddev = \vola dt -> vola*(HBivarVal (sqrt $ toYears dt,sqrt $ toYears dt))
  }
