import Base
import Psd

import Control.Applicative
import Data.List
import Text.Printf

import Numeric.GSL
import Numeric.GSL.ODE
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot

-- xdot t [x,v] = [v, -0.95*x-0.1*v]
fs = 1e6
n_init = 10 :: Int

adc x | x >  0 = 1
      | x <= 0 = -1

eventAtAlpha [u, y, v, fb, dac, toV,  toFb,  toDac,  start_time]
           = [u, y, v, fb, dac, toV', toFb', toDac', start_time']
  where
    toV'   = adc y
    toFb'  = toV'
    toDac' = step_dac * toFb'
    step_dac = 1.0
    start_time' = start_time + 1.0/fs

dU f a t | t < n_init' / fs = 0
         | otherwise  = a * w * cos (w * (t - n_init' / fs))
  where
    n_init' = fromIntegral n_init
    w = 2 * pi * f

xdot t [u , y , v ,  fb,  dac, toV, toFb, toDac, startTime]
     = [sU, sY, sV, sFb, sDac,   0,    0,     0,         0]
  where
    trf = 1e-9
    (osr, z0, p0, step_dac) = (256, 2*pi*fs, z0/100, 1.0)
    fsig = fs / 2 / osr

    sU   = dU fsig (0.7 * step_dac) (t + startTime)
    sV   = (toV - v) / trf
    sFb  = (toFb - fb) / trf
    sDac = (toDac - dac) / trf
    sY   = (-y + (u - dac)) * p0
    -- sY   = (-y + (u - dac) + (sU - sDac) / z0) * p0


xdotV :: Double -> Vector Double -> Vector Double
xdotV t vec = vector $ xdot t $ toList vec 

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

getLog :: [[Double]]
getLog = iterate (sim xdotV) (replicate 9 0)

main = do
  let fs  = 1e6
  let n_fft = 2^14 :: Int
  -- putStrLn . unlines . fmap showRecord . pick [8,0,1,2,3,4] $ take 128 getLog
  let log = getLog
  mplot . fmap vector . transpose $ 
    zipWith (:) (fmap fromIntegral [0 .. n_fft - 1])
                (pick [0,1,2] . take n_fft . drop n_init $ getLog)
                       
  mplot $ vector <$> [fromIntegral <$> [0 .. n_fft `div` 2 - 1],
                      fmap db10 . psdAbs . fmap (!!3) . take n_fft . drop n_init $ log]
  putStrLn "fin."
