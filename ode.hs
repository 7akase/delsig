import Control.Applicative
import Text.Printf

import Numeric.GSL
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot

-- xdot t [x,v] = [v, -0.95*x-0.1*v]
fs = 1e6
ts = linspace 10 (0,1e-6)

timeStep ini = odeSolve xdot (eventAtAlpha ini) ts

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

dU f a t = a * w * cos (w * t)
  where
    w = 2 * pi * f

xdot t [u , y , v ,  fb,  dac,  toV,  toFb,  toDac, start_time]
     = [sU, sY, sV, sFb, sDac, sToV, sToFb, sToDac, start_time]
  where
    trf = 1e-9
    (osr, z0, p0, step_dac) = (256, 2*pi*fs, z0/100, 1.0)
    fsig = fs / 2 / osr
    (sToV, sToFb, sToDac) = (0.0, 0.0, 0.0)
    sU   = dU fsig step_dac (t + start_time)
    sV   = (toV - v) / trf
    sFb  = (toFb - fb) / trf
    sDac = (toDac - dac) / trf
    sY   = (-y + (u - dac) + (sU - sDac) / z0) * p0

getUs :: [[Double]] -> [Double]
getUs = fmap (!! 0) 

-- main = mplot (ts:toColumns (scanl timeStep [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0] ts))
main = do
  let fs = 1e6
  let ts = take 500 [0..]
  let ss = iterate (last . toLists . timeStep) (replicate 9 0)
  -- putStrLn . unlines $ fmap (printf "%0.2f") $ getUs ss
  putStrLn . show . sum $ getUs (take 8 ss)
  -- let us  = log !! 0
  -- let ys  = log !! 1
  -- let vs =  log !! 2
  -- putStrLn . show $ length ts
  -- putStrLn . show $ length log 

  --  mplot $ vector <$> ts : us : []
  -- mplot $ vector <$> ts : vs : [] 
  putStrLn "fin."
