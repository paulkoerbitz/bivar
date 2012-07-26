module ClosedForm.Internal.Cdfs (
  stdNormalCdf
  ) where

import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)

stdNormalCdf :: Double -> Double
stdNormalCdf x = cumulative d x
  where d = normalDistr 0.0 1.0
