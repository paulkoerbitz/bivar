weightBvV :: Weight -> HBivarVal -> HBivarVal
weightBvV (Weight w) (HBivarVal (x,y)) = HBivarVal (w*x, w*y)

unconditional_expectations :: BsvParams -> (HBivarVal, HBivarVal) --Property -- BivarNode -> Property
unconditional_expectations paras = (unconditionalExp, movesExp)
    where 
      node             = BivarNode (0,0)
      m                = makeBsvModel paras
      g                = makeGrid m (toHomoskedastic m $ BivarVal (0.03, 100)) (HBivarVal (sqrt 3, sqrt 3)) 1.0 100
      startVals'       = fromGrid g node
      nodes            = triBivarMoves g eulerDiscr m node
      movesExp         = foldl (\acc (w, n') -> acc + weightBvV w (fromGrid g n')) (HBivarVal (0,0)) nodes
      unconditionalExp = (expectation eulerDiscr) startVals' (drift (process m) startVals') (Lattice.dt g)
      
moves' :: BsvParams -> [(Weight, BivarNode)]
moves' params = wNodes
  where 
    node             = BivarNode (0,0)
    m                = makeBsvModel params
    g                = makeGrid m (toHomoskedastic m $ BivarVal (0.00, 100)) (HBivarVal (sqrt 3, sqrt 3)) 1.0 100
    wNodes           = triBivarMoves g eulerDiscr m node

bsvParams :: BsvParams
bsvParams = BsvParams {ka = 1.0, th = startTheta, sr = 0.01, ss = 0.2, ro = 0.0} --0.22198166698217392}

matur  = 1.0 :: Double
    
nSteps = 200 :: Int

-- startIr = 0.0359 :: Double
-- startIr = 0.03 :: Double

startTheta = 0.03 :: Double

dynExact  = MC.makeBsvDynamics bsvParams

bsvModel  = makeBsvModel bsvParams

pc        = MC.PathChar {MC.n = 1, MC.dt = TimeDelta matur, MC.startVals = MC.MVal (BivarVal (unIR startIr, 100)) 0.0}

lat = Lattice {
  grid = grid',
  moves = triBivarMoves grid' eulerDiscr bsvModel
  }
  where grid' = makeGrid bsvModel (toHomoskedastic bsvModel (BivarVal (unIR startIr, 100))) (HBivarVal (sqrt 3, sqrt 3)) matur nSteps
        
cfBsvModel = makeBsvCfModel bsvParams
  
qlat = Lattice {
  grid = grid',
  moves = quadBivarMoves grid' cfBsvModel
  }
  where grid' = makeGrid bsvModel (toHomoskedastic bsvModel (BivarVal (unIR startIr, 100))) (HBivarVal (0.1, 0.1)) matur 1
        
maxWeight mvs' = foldl (\acc (Weight w,_) -> max acc w) 0 (mvs' (BivarNode (0,0)))

minWeight mvs' = foldl (\acc (Weight w,_) -> min acc w) 0 (mvs' (BivarNode (0,0)))
                     
maxNodes mvs' = foldl (\ ((minX,maxX),(minY,maxY)) (_,BivarNode (x,y)) -> ((min minX x,max maxX x),(min minY y,max maxY y))) ((1000,-1000),(1000,-1000)) (mvs' (BivarNode (0,0)))

--reduceNodes :: (Weight, BivarNode) -> (
reduceNodes :: [(Weight, BivarNode)] -> [(Int,Double)]
reduceNodes nds = Map.toList $ 
                  foldl (\ acc (Weight w, (BivarNode (x,_))) -> if (x `Map.member` acc) 
                                                                then Map.insertWith (+) x w acc
                                                                else Map.insert x w acc)
                  (Map.fromList []) nds
                  
                  
                  
toStr :: (Show a) => ((Int, Double) -> a) -> [(Int, Double)] -> String
toStr extrctr nds = intercalate "," $ map (show . extrctr) nds

toStr1 = toStr snd

toStr2 = toStr (\ (is,_) -> let (BivarVal (_,s)) = fromHomoskedastic bsvModel (fromGrid grd (BivarNode (0,is))) in s)

toStr3 = toStr (\ (ir,_) -> let (BivarVal (r,_)) = fromHomoskedastic bsvModel (fromGrid grd (BivarNode (ir,0))) in r)

nSamples = 100000 :: Int
         
grd = grid qlat

mvs' = moves qlat

sn = BivarNode (0,0)

expVal = (hMean cfBsvModel) (TimeDelta $ dt grd) (fromGrid grd (BivarNode (0,0)))
