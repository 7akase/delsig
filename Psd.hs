module Psd where

import Control.Applicative
import Data.Complex
import Numeric.FFT
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot
sample :: IO()
sample = do
  mplot $ vector <$> [0..255] : (fmap ((2/256*) . realPart . abs) $ fft $ (:+0) <$> sin . (2*pi/8*) <$> [0..255]) : []

log10 :: Double -> Double
log10 = logBase 10

db10 :: Double -> Double
db10 = (10 *) . log10

psdAbs :: [Double] -> [Double]
psdAbs xs = (fmap ((2/len*) . realPart . abs)) . take half_len . fft $ fmap (:+0) xs
  where
    half_len = length xs `div` 2
    len = fromIntegral $ length xs 
