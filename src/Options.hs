module Options (
  Instrument(..),
  Knockout(..),
  Barrier,
  makePayoff --,
--  euroTLO,
--  euroCall --,
--  zeroCouponBond
  ) where

import qualified Data.Map as Map

import Data.Hashable
import Types
import Lattice

instance Hashable DiffTime where
  hash dt' = hash $ toRational dt'
  

data Knockout = LbStockKnockout StockStrike
              | UbStockKnockout StockStrike
              | LbIrKnockout IrStrike
              | UbIrKnockout IrStrike

type Barrier = Map.Map DiffTime [Knockout]

data Instrument = EuroCall StockStrike DiffTime
                | EuroTLO IrStrike StockStrike DiffTime
                | BarrierEuroCall Barrier StockStrike DiffTime
                | BarrierTLO Barrier IrStrike StockStrike DiffTime
                | AmericanTLO IrStrike StockStrike DiffTime
                | ZeroCouponBond DiffTime
                  
noTouch :: BivarVal -> DiffTime -> Price -> Price
noTouch _ _ p = p

          
tloPayoff :: IrStrike -> StockStrike -> BivarVal -> Price
tloPayoff rK sK (BivarVal (r,s)) = Price $ max (unIrStrike rK - r) 0 * max (unStockStrike sK - s) 0

callPayoff :: StockStrike -> BivarVal -> Price
callPayoff sK (BivarVal (_,s)) = Price $ max (s - unStockStrike sK) 0
          
makeEuroCall :: Instrument -> Option BivarVal DiffTime
makeEuroCall (EuroCall sK mat) = Option {
  maturity = mat,
  payoff = callPayoff sK,
  transition = noTouch
  }
                 
makeEuroTLO :: Instrument -> Option BivarVal DiffTime
makeEuroTLO (EuroTLO rK sK mat) = Option {
  maturity = mat,
  payoff = tloPayoff rK sK,
  transition = noTouch
  }
                                  
makeAmericanTLO :: Instrument -> Option BivarVal DiffTime
makeAmericanTLO (AmericanTLO rK sK mat) = Option {
  maturity = mat,
  payoff = tloPayoff rK sK,
  transition = \v _ p -> max p (tloPayoff rK sK v)
  }
           
makeZCB :: Instrument -> Option BivarVal DiffTime
makeZCB (ZeroCouponBond mat) = Option {
  maturity = mat,
  payoff = \_ -> Price 1,
  transition = noTouch
  }

checkKnockout :: (Price, BivarVal) -> Knockout -> Price
checkKnockout (p, BivarVal (_,s)) (LbStockKnockout sK) = if unStockStrike sK > s then (Price 0) else p
checkKnockout (p, BivarVal (_,s)) (UbStockKnockout sK) = if unStockStrike sK < s then (Price 0) else p
checkKnockout (p, BivarVal (r,_)) (LbIrKnockout    rK) = if unIrStrike    rK > r then (Price 0) else p
checkKnockout (p, BivarVal (r,_)) (UbIrKnockout    rK) = if unIrStrike    rK < r then (Price 0) else p

barrierTrans :: Barrier -> BivarVal -> DiffTime -> Price -> Price
barrierTrans barrier v t p = if Map.member t barrier
                             then let knockouts = barrier Map.! t
                                  in foldl (\p' k -> checkKnockout (p',v) k) p knockouts
                             else p

makeBarrierEuroCall :: Instrument -> Option BivarVal DiffTime
makeBarrierEuroCall (BarrierEuroCall barrier sK mat) = Option {
  maturity   = mat,
  payoff     = callPayoff sK,
  transition = barrierTrans barrier
  }
                            
makeBarrierTLO :: Instrument -> Option BivarVal DiffTime
makeBarrierTLO (BarrierTLO barrier rK sK mat) = Option {
  maturity = mat,
  payoff = tloPayoff rK sK,
  transition = barrierTrans barrier
  }
                 
makePayoff :: Instrument -> Option BivarVal DiffTime
makePayoff i@(ZeroCouponBond _)      = makeZCB i
makePayoff i@(EuroTLO _ _ _)         = makeEuroTLO i
makePayoff i@(BarrierTLO _ _ _ _)    = makeBarrierTLO i
makePayoff i@(AmericanTLO _ _ _)     = makeAmericanTLO i
makePayoff i@(EuroCall _ _)          = makeEuroCall i
makePayoff i@(BarrierEuroCall _ _ _) = makeBarrierEuroCall i