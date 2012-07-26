module DemoData (
  DataPoint(..),
  EvalMethod(..),
  tables
  ) where

import Lattice
import Data.List
import Types
import Options

import qualified Data.Map as Map
import Data.Time.Clock

bsvParams = BsvParams {ka=0.25, th=0.04, sr=0.02, ss=0.2, ro=0.0} :: BsvParams

type DataPoint = (EvalMethod, Double, IrStrike, StockStrike) 

data EvalMethod = ClosedForm
                | Tree Int
                | Quad Int Double
                | MCExact Int Int
                | MCEuler Int Int
                deriving (Show)
                         
secsPerMonth = 365*24*3600 `div` 12

knockouts = [UbStockKnockout (StockStrike 150), UbIrKnockout (IrStrike 0.6)]
                         
barrier :: Barrier
barrier = Map.fromList [(secondsToDiffTime (i*secsPerMonth), knockouts) | i <- [1..12]]
                         
options :: [Instrument]
options = [ EuroTLO     (IrStrike 0.03) (StockStrike 100) (fromYears 1.0)
          , BarrierTLO  barrier (IrStrike 0.03) (StockStrike 100) (fromYears 1.0)
          , AmericanTLO (IrStrike 0.03) (StockStrike 100) (fromYears 1.0)
          ] 

table1Cols = [(IrStrike 0.04, StockStrike 110),
              (IrStrike 0.03, StockStrike 100),
              (IrStrike 0.02, StockStrike  90)]
            
-- table1Rows = [(evalMethod, ro) | evalMethod <- [ClosedForm, Tree 48, Tree 96, Tree 192, Quad 1 0.4, Quad 1 0.2, Quad 1 0.1], ro <- [-0.25, 0, 0.25]]
table1Rows = [(evalMethod, ro) | evalMethod <- [ClosedForm, Quad 1 0.005], ro <- [-0.25, 0, 0.25]]

table2Rows = [(evalMethod, ro) | evalMethod <- [MCExact 1000000 12, Tree 48, Tree 96, Tree 192, Quad 1 0.1, Quad 1 0.05, Quad 1 0.01], ro <- [-0.25, 0, 0.25]]

table3Rows = [(evalMethod, ro) | evalMethod <- [MCEuler 1000000 100, Tree 48, Tree 96, Tree 192], ro <- [-0.25, 0, 0.25]]

oneYear :: DiffTime
oneYear = secondsToDiffTime (365*24*3600)



table1a :: (String, Instrument, Parameters, [[DataPoint]], Double)
table1a = ("European Call, K=100, T=1yr", 
           EuroCall (StockStrike 0) oneYear,
           Left bsvParams,
           [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table1Rows],
           1)

table1b :: (String, Instrument, Parameters, [[DataPoint]], Double)
table1b = ("European Traffic Light Option, rK=0.03, sK=100, T=1yr", 
           EuroTLO (IrStrike 0.0) (StockStrike 0) oneYear, 
           Left bsvParams,           
          [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table1Rows],
          100)

table2a :: (String, Instrument, Parameters, [[DataPoint]], Double)
table2a = ("European Call with barrier, sK=100, monthly checked barrier at 150, T=1yr", 
           BarrierEuroCall barrier (StockStrike 100) oneYear, 
           Left bsvParams,
           [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table2Rows],
           1)

table2b :: (String, Instrument, Parameters, [[DataPoint]], Double)
table2b = ("Barrier Traffic Light Option, rK=0.03, sK=100, T=1yr", 
           BarrierTLO barrier (IrStrike 0.03) (StockStrike 100) oneYear,
           Left bsvParams,
          [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table2Rows],
          100)
          
table3a :: (String, Instrument, Parameters, [[DataPoint]], Double)
table3a = ("European Call, K=100, T=1yr, alt. model",
           EuroCall (StockStrike 100) oneYear,
           Left bsvParams,
           [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table3Rows],
           1)
         
table3b :: (String, Instrument, Parameters, [[DataPoint]], Double)
table3b = ("European TLO, rK=0.03, sK=100, T=1yr, alt. model",
           EuroTLO (IrStrike 0.03) (StockStrike 100) oneYear,
           Left bsvParams,           
           [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table3Rows],
           100)

tables = [table1a, table1b, table2a, table2b] --, table3a, table3b]

-- table3 :: (Instrument, [[DataPoint]])
-- table3 = (AmericanTLO (IrStrike 0.03) (StockStrike 100),
--           [[(em, ro, rStrike, sStrike) | (rStrike, sStrike) <- table1Cols] | (em, ro) <- table1Rows])

-- This is different
-- table4 :: (Instrument, [[DataPoint]])
-- table4 = undefined