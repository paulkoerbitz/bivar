{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
module Main (
  main
  ) where

import Control.Monad
import System.Environment
import qualified Data.Time.Clock as Clock

import Types
import Lattice
import BivarTrinomialTree
import BivarQuad
import Options
import qualified MonteCarlo.BlackScholesVasicekMC as MC
import ClosedForm.BlackScholesVasicekCall
import ClosedForm.BlackScholesVasicekTLO
import ClosedForm.VasicekZeroCouponBond
import DemoData
import Numeric (showFFloat, showGFloat)

import Data.List
import Text.Printf

evalCf :: Instrument -> BsvParams -> Price
evalCf (EuroTLO rK sK mat)  p = blackScholesVasicekTLO  p rK sK startIr startS mat
evalCf (EuroCall sK mat)    p = blackScholesVasicekCall p sK startS startIr mat
evalCf (ZeroCouponBond mat) p = zcbPrice p startIr mat

updateStrikes :: Instrument -> IrStrike -> StockStrike -> Instrument
updateStrikes (EuroTLO _ _ mat) rK sK        = EuroTLO rK sK mat
updateStrikes (EuroCall _ mat) _ sK          = EuroCall sK mat
updateStrikes (ZeroCouponBond mat) _ _       = ZeroCouponBond mat
updateStrikes (BarrierTLO b _ _ mat) rK sK   = BarrierTLO b rK sK mat
updateStrikes (BarrierEuroCall b _ mat) _ sK = BarrierEuroCall b sK mat

evalMC :: MC.Dynamics -> Int -> Int -> BivarVal -> Opt2D -> Price
evalMC dyn nPaths nSteps sv opt =
  let sv'                 = MC.MVal sv 0.0
      pc                  = MC.PathChar nSteps (Lattice.maturity opt / Clock.secondsToDiffTime (toInteger nSteps)) sv'
      MC.McResult res err = MC.computeMcResult pc dyn opt nPaths
  in Price res
     
evalTree :: Parameters -> Int -> BivarVal -> Opt2D -> Price
evalTree p n sv option = fastTBT' lat option
  where 
    stepSize = HBivarVal (sqrt 3, sqrt 3) -- needs adoption
    m        = makeEitherModel p
    gridSv   = toHomoskedastic m sv
    tgrid    = makeGrid m gridSv stepSize (Lattice.maturity option) n
    lat      = Lattice {grid=tgrid, moves=triBivarMoves tgrid eulerDiscr m}
    
evalQuad :: BsvParams -> Int -> Double -> BivarVal -> Opt2D -> Price
evalQuad p n dx sv option = fastTBT' lat option
  where 
    stepSize   = HBivarVal (dx, dx)
    cfBsvModel = makeBsvCfModel p
    m          = model cfBsvModel
    gridSv     = toHomoskedastic m sv
    grid'      = makeGrid m gridSv stepSize (Lattice.maturity option) n
    lat        = Lattice {grid=grid', moves=quadBivarMoves grid' cfBsvModel}

evalCell :: Parameters -> Instrument -> DataPoint -> Price
evalCell p i (em, ro', rK, sK) = case (newP,i,em) of
  (Left p', ZeroCouponBond mat, ClosedForm           ) -> zcbPrice p' startIr mat
  (Left p', EuroCall _  mat   , ClosedForm           ) -> blackScholesVasicekCall p' sK startS startIr mat
  (Left p', EuroTLO _ _ mat   , ClosedForm           ) -> blackScholesVasicekTLO p' rK sK startIr startS mat
  (Left p', _                 , Quad nSteps dx       ) -> evalQuad p' nSteps dx sv option
  (Left p', _                 , MCExact nPaths nSteps) -> evalMC (MC.makeBsvDynamics p') nPaths nSteps sv option
  (_      , _                 , Tree nSteps          ) -> evalTree newP nSteps sv option
  (Left p', _                 , MCEuler nPaths nSteps) -> evalMC (MC.makeBsvEulerDynamics p') nPaths nSteps sv option  
  (_      , _                 , MCEuler nPaths nSteps) -> evalMC (MC.makeEulerDynamics (makeEitherModel newP)) nPaths nSteps sv option
  where 
    newP   = case p of (Left  p') -> Left $ p'{ro=ro'}
                       (Right p') -> let bsv' = (bsv p'){ro=ro'} in Right $ p'{bsv=bsv'}
    sv     = BivarVal (unIR startIr, unStock startS)
    option = makePayoff (updateStrikes i rK sK)

time :: IO a -> IO (a,Clock.NominalDiffTime)
time computation = 
  Clock.getCurrentTime  >>= \start ->
  computation >>= \ !result -> 
  Clock.getCurrentTime  >>= \end -> 
  return (result, Clock.diffUTCTime end start)
  
startS = Stock 100
startIr = IR 0.03
maturity = fromYears 1.0

toFloat :: Clock.NominalDiffTime -> Double
toFloat x = fromRational $ toRational x

timedEvalCell :: Parameters -> Instrument -> DataPoint -> IO (Price, Clock.NominalDiffTime)
timedEvalCell paras option cell = time $ return $ evalCell paras option cell

timedEvalTable :: Parameters -> Instrument -> [[DataPoint]] -> IO [[(Price, Clock.NominalDiffTime)]]
timedEvalTable paras option table = mapM (mapM (timedEvalCell paras option)) table 

-- assumes special structure of table
computeErrors :: [[(Price, Clock.NominalDiffTime)]] -> [[(Price, Double, Clock.NominalDiffTime)]]
computeErrors tbl = let refVals = cycle $ map (map fst) (take 3 tbl)
                    in map (\(row,refrow) -> map (\((Price p, t),Price refp) -> (Price p, (p-refp)/refp, t)) (zip row refrow)) (zip tbl refVals)

showTableEntry :: (Price, Double, Clock.NominalDiffTime) -> Double -> String
showTableEntry (Price p, err, t) multiplier = 
  (showFFloat (Just 3) (p*multiplier)) " & " ++
  (showFFloat (Just 2) (100*err)) "\\% & " ++ 
  (showGFloat (Just 3) (toFloat t)) "s"
  
showTable :: (String, Instrument, Parameters, [[DataPoint]], Double) -> IO String
showTable (name, option, paras, dataPoints, multiplier) = 
  timedEvalTable paras option dataPoints >>= \tbl -> 
  return $ name ++ "\n" ++ (intercalate "\n" $ map (\row -> intercalate " & " $ map (\cell -> showTableEntry cell multiplier) row) (computeErrors tbl))
  
readIntArg :: [String] -> Maybe Int
readIntArg (x:_) = case reads x of [(a,_)] -> Just a
                                   _       -> Nothing
readIntArg _      = Nothing

  
main :: IO ()
main = getArgs >>= \args ->  
  showTable (DemoData.tables !! (read (head args))) >>= putStrLn
