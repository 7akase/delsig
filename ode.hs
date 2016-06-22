import Base
import Psd
import Delsig
import Dsm1

import Control.Applicative
import Data.List
import Text.Printf

import Numeric.GSL
import Numeric.GSL.ODE
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot

n_init = 10 :: Int

sim :: (Double -> Vector Double -> Vector Double) -> [Double] -> [Double]
-- | simulate xdotV for single timeslot
sim xdotV init_cond = 
  last . toLists $ odeSolveV RKf45 
                             init_step abstol reltol 
                             xdotV 
                             init_cond' 
                             ts 
    where
      init_step = 1e-9
      abstol = 1e-3
      reltol = 1e-6
      init_cond' = vector $ eventAtAlpha init_cond
      ts = vector [0, 1.0 / fs]

u' :: Double -> Double
u' t | t < fromIntegral n_init / fs = 0.0
     | otherwise = (0.7 * step_dac) * w * cos (w * (t - t_init))
       where
         w = 2 * pi * fsig
         fsig = fs / 2 / fromIntegral osr
         t_init = fromIntegral n_init / fs

getLog :: [[Double]]
getLog = iterate (sim (xdotV u')) (replicate 9 0)

main :: IO ()
main = do
  let fs  = 1e6
  let n_fft = 2^8 :: Int
  let n_fft' = fromIntegral n_fft
  let log = getLog
  mplot . fmap vector . transpose $ 
    zipWith (:) (fmap fromIntegral [0 .. n_fft - 1])
                (pick [0,1,2] . take n_fft . drop n_init $ getLog)
                       
  mplot $ vector <$> [fromIntegral <$> [0 .. n_fft `div` 2 - 1],
                      fmap db10 . psdAbs . fmap (!!3) . take n_fft . drop n_init $ log]
  putStrLn "fin."

sample2 :: IO()
sample2 = do
  let n_fft = 2^12
  let n_fft' = fromIntegral n_fft
  let ys = psdAbs . fmap (!! 3) . 
             take n_fft . drop n_init $ getLog
  let freqs = (fs / n_fft' *) . fromIntegral <$> [0 .. n_fft `div` 2 - 1]
  putStrLn . printf "%0.2f" . db10 $ calculateSNR ys (round (fromIntegral osr / 2)) 2 
  mplot $ fmap vector [freqs, fmap db10 ys]

sample1 :: IO ()
sample1 = do
  -- |
  -- >>> sample1
  -- 24.64
  let n_fft = 2^14
  let snr = db10 $ calculateSNR (psdAbs (fmap (sin . (2*pi/n_fft*16*)) [0..n_fft-1])) 16 2 
  putStrLn $ printf "%0.2f" snr

