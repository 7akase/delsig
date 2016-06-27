module Delsig where

import Control.Applicative
import Text.Printf
import Data.List
import Data.Ix
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Psd

type VecEle = Vector Double

-- view :: String -> [VecEle] -> IO ()
view fmt = putStrLn . unlines . fmap (concat . (fmap (printf fmt)))
-- |
-- >>> view "%02d " [[0,1],[2,3]]
-- 00 01 
-- 02 03
calculateSNR :: [Double] -> Int -> Int -> Double
calculateSNR hwfft f nsig =
  dbp $ s / n
    where
      len = length hwfft
      signalBins = filter (inRange (0,len)) [f-nsig+1..f+nsig]
      s = sum $ fmap (hwfft !!) signalBins
      noiseBins  = [0..len-1] \\ signalBins
      n = norm $ fmap (hwfft !!) noiseBins
      
      
hann :: Int -> [Double]
hann n = (0.5 *) . (1.0 -) . cos . (2*pi*) <$> xs
  where
    n' = fromIntegral n :: Double
    xs = (/ (n' - 1)) <$> [0 .. n' - 1]


-- support funciton
norm :: [Double] -> Double
norm = sum . fmap (^ 2)

dbv :: Double -> Double
dbv x = 20 * log x / log 10

dbp :: Double -> Double
dbp x = 10 * log x / log 10

  
