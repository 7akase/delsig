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
w = 2 * pi * fsig
fsig = fs / 2 / fromIntegral (osr - 2)
t_init = fromIntegral n_init / fs

-- | simulate xdotV for single timeslot
responseOf f = iterate (next f) $ replicate 5 0

u :: TIME -> U
u t | t < fromIntegral n_init = 0.0
    | otherwise = 0.7 * step_dac * sin (w * (t - t_init))

sample1 :: IO ()
sample1 = do
  let n_fft = 2^13 :: Int
  let df = fs / fromIntegral n_fft
  let f = sin . (w *)
  let ys = psdAbs . (!! 1) . transpose . take n_fft . drop n_init $ responseOf f 
  let freqs = (df *) . fromIntegral <$> [0 .. n_fft `div` 2 - 1]
  mplot $ fmap vector [freqs, fmap db10 ys]

sample3 :: IO ()
sample3 = do
  let n_fft = 2^12 :: Int
  let df = fs / fromIntegral n_fft
  let f = u 
  let log = transpose . take n_fft . drop n_init $ responseOf f 
  let freqs = (df *) . fromIntegral <$> [0 .. n_fft `div` 2 - 1]
  let ts = (1.0/fs *) . fromIntegral <$> [0 .. n_fft - 1]
  mplot $ fmap vector (take 3 log)
  let snr = calculateSNR (take (n_fft `div` osr) (psdAbs (log !! 1))) 
                         (n_fft `div` osr)
                         2
  putStrLn $ printf "%0.2f" snr

sample2 :: IO ()
sample2 = do
  -- |
  -- >>> sample1
  -- 24.64
  let n_fft = 2^14
  let snr = db10 $ calculateSNR (psdAbs (fmap (sin . (2*pi/n_fft*16*)) [0..n_fft-1])) 16 2 
  putStrLn $ printf "%0.2f" snr
