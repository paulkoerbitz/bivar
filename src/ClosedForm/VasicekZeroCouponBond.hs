module ClosedForm.VasicekZeroCouponBond (
  zcbPrice
  ) where

import Types
          
zcbPrice :: BsvParams -> IR -> DiffTime -> Price
zcbPrice (BsvParams k t s _ _) (IR r0) dt' =
  Price $ exp ((t - s*s/(2*k*k))*(psiL - dt) - s*s/(4*k)*psiL*psiL - psiL*r0)
    where 
      dt   = toYears dt'      
      psiL = (1- exp (-k*dt)) / k

