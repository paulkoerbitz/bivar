module BivarQuad (
  quadBivarMoves,
  getMaxNodes
  ) where

import Lattice
import Types
import BivarTrinomialTree (closestNode)
import Debug.Trace

-- needs >= 5 and then odd number of points ([5,7,..])
compositeSimpson :: Int -> Double -> [Double]
compositeSimpson n h = h/3 : (take (n-2) (cycle [4*h/3, 2*h/3]) ++ [h/3])

-- needs exactly 4 points
simpsons38 :: Int -> Double -> [Double]
simpsons38 n h = 3*h/8 : (replicate (n-2) (9*h/8)) ++ [3*h/8]

-- needs >= 8 points
extendedSimpson :: Int -> Double -> [Double]
extendedSimpson n h = 17*h/48 : 59*h/48 : 43*h/48 : 49*h/48 : replicate (n-8) h ++ [49*h/48,43*h/48,59*h/48,17*h/48]

-- needs >= 2 points
trapezoidal :: Int -> Double -> [Double]
trapezoidal n h = (h/2) : (replicate (n-2) h) ++ [h/2]

intWeights :: Int -> Double -> [Double]
intWeights n h | n == 4                     = simpsons38 n h
               | n >= 5 && mod (n+1) 2 == 0 = compositeSimpson n h
               | n >= 8                     = extendedSimpson n h
               | otherwise                  = trapezoidal n h
    

-- we'll do this based on standard devs for now ...
getMaxNodes :: Grid BivarVal HBivarVal DiffTime Int BivarNode -> CfModel BivarVal HBivarVal DiffTime -> BivarNode -> (BivarNode,BivarNode)
getMaxNodes g m n = (minNodes, maxNodes)
  where 
    curHVal  = fromGrid g n
    stdDevs  = hStd m (dt g)
    minVals  = curHVal - 6*stdDevs
    minNodes = closestNode g minVals - (BivarNode (1,1))
    maxVals  = curHVal + 6*stdDevs
    maxNodes = closestNode g maxVals + (BivarNode (1,1))
    
-- exactExpectation :: CfModel BivarVal HBivarVal Double -> HBivarVal -> Double -> HBivarVal
-- exactExpectation m val dt = 
--   where
--     prcs = process (model m)

quadBivarMoves :: Grid BivarVal HBivarVal DiffTime Int BivarNode -> CfModel BivarVal HBivarVal DiffTime -> BivarNode -> [(Weight,BivarNode)]
quadBivarMoves g m n = map (\(w,n') -> (Weight (w*f n'), n')) $ zip weights nodes
  where
    curVal                      = fromGrid g n
    expVal                      = (hMean m) (dt g) curVal
    mid                         = closestNode g expVal
    (lbi@(BivarNode (lbiX,lbiY)),ubi@(BivarNode (ubiX,ubiY))) = getMaxNodes g m mid
    lbH@(HBivarVal (lbHX,lbHY)) = fromGrid g lbi
    ubH@(HBivarVal (ubHX,ubHY)) = fromGrid g ubi
    nodes                       = [BivarNode (x,y) | x <- [lbiX..ubiX], y <- [lbiY..ubiY]]
    xWeights                    = intWeights (ubiX-lbiX+1) ((ubHX-lbHX) / fromIntegral (ubiX-lbiX)) 
    yWeights                    = intWeights (ubiY-lbiY+1) ((ubHY-lbHY) / fromIntegral (ubiY-lbiY))
    weights                     = [wx*wy | wx <- xWeights, wy <- yWeights]
    f                           = \n' -> let dt' = dt g
                                             h0  = fromGrid g n 
                                             h1  = fromGrid g n' 
                                         in (hDensity m dt' h0 h1) * (hDiscount m dt' h0 h1)


