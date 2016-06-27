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
import Graphics.Gnuplot.Simple

import Data.Numbers.Primes

n_init = 10 :: Int
t_init = fromIntegral n_init / fs

n_fft = 2^14
df = fs / fromIntegral n_fft
bin_bw  = n_fft `div` (2 * osr)
bin_sig = last . takeWhile (< bin_bw - 3) $ primes 
n_sig = (fromIntegral n_fft) / (fromIntegral bin_sig) 
w_sig = 2 * pi * fs / n_sig

-- | simulate xdotV for single timeslot
responseOf f = iterate (next f) $ replicate 5 0

u :: TIME -> U
u t | t < fromIntegral n_init = 0.0
    | otherwise = 0.7 * step_dac * sin (w_sig * (t - t_init))

getLog :: [[Double]]
getLog = transpose . take n_fft . drop n_init $ responseOf u

sample1 :: IO ()
sample1 = do
  let n_fft = 2^13 :: Int
  let df = fs / fromIntegral n_fft
  let f = sin . (w_sig *)
  let ys = psdAbs . (!! 1) . transpose . take n_fft . drop n_init $ responseOf f 
  let freqs = (df *) . fromIntegral <$> [0 .. n_fft `div` 2 - 1]
  mplot $ fmap vector [freqs, fmap db10 ys]

sample3 :: IO ()
sample3 = do
  let n_fft = 2^12 :: Int
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
  let snr = db10 $ calculateSNR (psdAbs (fmap (sin . (w_sig *) . fromIntegral) [0..n_fft-1])) bin_sig 2 
  putStrLn $ printf "%0.2f" snr

snrSig :: Double
snrSig = (\xs -> calculateSNR xs bin_sig 2) . take bin_bw . psdAbs $
         sin <$> (w_sig *) <$> fromIntegral <$> [0..n_fft-1]

snrMod :: Double
snrMod = (\xs -> calculateSNR xs bin_sig 2) . take bin_bw . psdAbs $
         getLog !! 1 

main :: IO ()
main = do
  let attr = [Custom "logscale x" []]
  let log = getLog
  let freqs = (df *) . fromIntegral <$> [0 .. n_fft `div` 2 - 1]
  let psd = fmap db10 . psdAbs $ log !! 1 
  mplot . fmap vector $ choose [0,2] log
  plotPath attr $ zip freqs psd
 
