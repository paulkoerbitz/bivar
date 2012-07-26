module ClosedForm.BlackScholesVasicekCall (
  blackScholesVasicekCall,
  RabinovichTestCase(..),
  rabinovichTestValues
  ) where

import Types

import ClosedForm.VasicekZeroCouponBond
import ClosedForm.Internal.Cdfs
        
blackScholesVasicekCall :: BsvParams -> StockStrike -> Stock -> IR -> DiffTime -> Price
blackScholesVasicekCall p@(BsvParams ka th sr ss ro) (StockStrike k) (Stock s0) (IR r0) dt' =
  Price $ s0*n1 - k*zcb*n2
    where 
      dt          = toYears dt'
      psi         = (1 - exp (-ka*dt)) / ka
      (Price zcb) = zcbPrice p (IR r0) dt'
      t           = ss*ss*dt + (dt - 2*psi + (1 - exp(-2*ka*dt))/(2*ka))*(sr/ka)*(sr/ka) + 2*ro*ss*(dt-psi)*sr/ka
      d1          = (log (s0/(k*zcb)) + 0.5*t) / sqrt t
      d2          = d1 - sqrt t
      n1          = stdNormalCdf d1
      n2          = stdNormalCdf d2

data RabinovichTestCase = RabinovichTestCase {
  p :: BsvParams,
  r0 :: IR,
  s0 :: Stock,
  sK :: StockStrike,
  res :: Price
  }

rabinovichTestValues = 
  [ RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.09, ss=0.2, ro=( 1.0)}) (Mpor (-0.2)), r0 = IR 0.15, s0 = Stock 35, sK = StockStrike 50, res = Price  0.30 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.4, th=0.1, sr=0.05, ss=0.2, ro=( 1.0)}) (Mpor (-0.2)), r0 = IR 0.10, s0 = Stock 35, sK = StockStrike 50, res = Price  0.25 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.03, ss=0.2, ro=( 0.8)}) (Mpor (-0.2)), r0 = IR 0.10, s0 = Stock 35, sK = StockStrike 50, res = Price  0.31 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.03, ss=0.2, ro=(-1.0)}) (Mpor (-0.2)), r0 = IR 0.15, s0 = Stock 35, sK = StockStrike 50, res = Price  0.74 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.4, th=0.1, sr=0.05, ss=0.2, ro=(-1.0)}) (Mpor (-0.2)), r0 = IR 0.10, s0 = Stock 35, sK = StockStrike 50, res = Price  0.56 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.07, ss=0.2, ro=(-0.8)}) (Mpor (-0.2)), r0 = IR 0.10, s0 = Stock 35, sK = StockStrike 50, res = Price  0.63 }
  
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.09, ss=0.1, ro=( 1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 50, sK = StockStrike 50, res = Price  1.83 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.05, ss=0.1, ro=( 1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 50, sK = StockStrike 50, res = Price  2.03 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.07, ss=0.1, ro=(-0.8)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 50, sK = StockStrike 50, res = Price  3.10 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.09, ss=0.1, ro=(-1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 50, sK = StockStrike 50, res = Price  3.44 }
  
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.09, ss=0.3, ro=( 1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 60, sK = StockStrike 50, res = Price 12.77 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.01, ss=0.1, ro=( 0.2)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 60, sK = StockStrike 50, res = Price 10.81 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.01, ss=0.1, ro=(-1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 60, sK = StockStrike 50, res = Price 10.83 }
  , RabinovichTestCase {p = toRiskNeutral (BsvParams {ka=0.1, th=0.1, sr=0.09, ss=0.2, ro=(-1.0)}) (Mpor (-0.2)), r0 = IR 0.01, s0 = Stock 60, sK = StockStrike 50, res = Price 12.57 }
  ]
