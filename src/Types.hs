{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
  IrStrike(..),
  StockStrike(..),
  Stock(..),
  IR(..),
  StockStd(..),
  IRStd(..),
  StockVol(..),
  DiffTime(..),
  toYears,
  fromYears,
  Price(..),
  BsvParams(..),
  CevCklsParams(..),
  Parameters(..),
  Mpor(..),
  BivarNode(..),
  BivarVal(..),
  HBivarVal(..),
  toRiskNeutral
  ) where

import Data.Time.Clock

newtype IrStrike = IrStrike {unIrStrike :: Double} deriving (Show)
newtype StockStrike = StockStrike {unStockStrike :: Double} deriving (Show)
newtype Stock = Stock {unStock :: Double} deriving (Eq, Num, Show)
newtype IR = IR {unIR :: Double} deriving (Eq, Num, Show)
newtype IRStd = IRStd {unIRStd :: Double } deriving (Eq, Num, Show)
newtype StockStd = StockStd {unStockStd :: Double } deriving (Eq, Num, Show)
newtype StockVol = StockVol Double
newtype Price = Price {unPrice :: Double} deriving (Eq, Show, Num, Ord)

newtype Mpor = Mpor Double

data BsvParams = BsvParams { ka :: Double, th :: Double, sr :: Double, ss :: Double, ro :: Double } deriving (Show)

data CevCklsParams = CevCklsParams { bsv :: BsvParams, xi :: Double, al :: Double } deriving (Show)

type Parameters = Either BsvParams CevCklsParams

instance (Num a, Num b) => Num (a,b) where 
  (+) (x1,y1) (x2,y2) = (x1+x2, y1+y2)
  (-) (x1,y1) (x2,y2) = (x1-x2, y1-y2)
  (*) (x1,y1) (x2,y2) = (x1*x2, y1*y2)
  abs (x1,y1) = (abs x1, abs y1)
  fromInteger i = (fromInteger i, fromInteger i)
  signum (x1,y1) = (signum x1, signum y1)
  

newtype BivarNode = BivarNode {unBivarNode :: (Int,Int)} deriving (Show, Eq, Num, Ord)
newtype BivarVal = BivarVal {unBivarVal :: (Double, Double)} deriving (Show, Eq, Num, Ord)
newtype HBivarVal = HBivarVal {unHBivarVal :: (Double, Double)} deriving (Show, Eq, Num, Ord)


toRiskNeutral :: BsvParams -> Mpor -> BsvParams
toRiskNeutral m (Mpor l) = BsvParams (ka m) newTheta (sr m) (ss m) (ro m)
  where newTheta = (th m) - (sr m)*l / (ka m)


--type TimeDelta = Clock.DiffTime

toYears :: DiffTime -> Double
toYears x = fromRational $ toRational x / 31536000  --  31536000 = 365*24*3600

fromYears :: (RealFrac a) => a -> DiffTime
fromYears y = secondsToDiffTime (round (y*31536000))