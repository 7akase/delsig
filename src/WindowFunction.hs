module WindowFunction where

import Control.Applicative
import Text.Printf
import Data.List
import Data.Ix
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Psd

type VecEle = Vector Double
newtype Wave = Wave { getList :: [Double] } deriving (Show)

instance Num Wave
  xs + ys
  xs * ys 
  abs xs
  signum xs
  fromInteger xs
  negate xs

instance Fractional Wave where
  xs / ys = Wave $ zipWith (/) (getList xs) (getList ys)
{-
instance Fractional t => Fractional ([] t) where
  (/) as bs = zipWith (/) as bs 
-}
hann n = (0.5 *) . (1.0 -) . cos . (2*pi*) <$> xs
  where
    n' = fromIntegral n :: Double
    xs = (/ (n' - 1)) <$> [0 .. n' - 1]

blackmann :: Int -> [Double]
blackmann n = [0.42 - 0.5 * cos (2*pi*x) + 0.08 * cos (4*pi*x)
                | x <- [0 .. fromIntegral n - 1]]

-- support funciton
norm :: [Double] -> Double
norm = sum . fmap (^ 2)

dbv :: Double -> Double
dbv x = 20 * log x / log 10

dbp :: Double -> Double
dbp x = 10 * log x / log 10

  
