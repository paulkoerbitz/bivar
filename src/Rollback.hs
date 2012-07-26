
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa as R

import Types

moves :: [(Double,Int)]
moves = [(1/3,-1),(1/3,0),(1/3,1)]

data Grid' = Grid' {values :: U.Vector Double, midIndex :: Int} deriving (Show)

(~!) :: Grid' -> Int -> Double
(~!) (Grid' vals mid) i = vals U.! (i+mid)

rollback :: Grid' -> Grid'
rollback g@(Grid' vls mid) = Grid' vls' mid'
  where 
    vls'   = U.map (\i -> foldl (\acc (w,off) -> acc + w*(g ~! (i+off))) 0 moves) (U.enumFromTo (-mid') (lstIdx-mid'))
    --fstIdx = 0 - (snd (head moves))
    lstIdx = U.length vls - 3 --- (snd (last moves)) + (snd (head moves))
    mid'   = mid-1
        


data Plane = Plane {plValues :: R.Array R.DIM2 Double,  plMidIndex :: BivarNode} deriving (Show)

(!.) :: Plane -> BivarNode -> Double
(!.) (Plane vals mid) idx = vals R.! (R.Z R.:. ix R.:. iy)
  where BivarNode (ix,iy) = idx + mid

moves' :: [(Double,BivarNode)]
moves' = [(1/9,BivarNode (x,y)) | x <- [-1..1], y <- [-1..1]]

rollback' ::Plane -> Plane
rollback' pl@(Plane vls mid) = Plane vls' mid'
  where 
    vls'       = R.fromList (R.Z R.:. (7::Int) R.:. (7::Int)) $ map (\i -> foldl (\acc (w,off) -> acc + w*(pl !. (i+off))) 0 moves')  relIndices
    relIndices = [BivarNode (x,y) | x <- [-3..3], y <- [-3..3]]
    --lstIdx     = U.length vls - 3 --- (snd (last moves)) + (snd (head moves))
    mid'       = BivarNode (3,3)
    
main = putStrLn $ show $ rollback' $ Plane {plValues = R.fromList (R.Z R.:. (9::Int) R.:. (9::Int)) [0..80], plMidIndex = BivarNode (4,4)}

