import Control.Applicative
import Numeric.GSL
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot

-- xdot t [x,v] = [v, -0.95*x-0.1*v]

ts = linspace 100 (0,1e-6)

-- timeStep ini = odeSolve xdot (eventAtAlpha ini) ts
timeStep ini = odeSolve xdot ini ts

adc x | x >  0 = 1
      | x <= 0 = -1

eventAtAlpha [u, y, v, fb, dac, toV, toFb, toDac] = [u, y, v, fb, dac, toV', toFb', toDac']
  where
    toV'   = adc y
    toFb'  = toV'
    toDac' = step_dac * toFb'
    step_dac = 1.0
{-
dU f a t | t < 1.0 / f  = 0.0 
         | otherwise    = a * w * cos (w * t) 
           where
             w = 2 * pi * f
-}
dU f a t = a * w * cos (w * t)
  where
    w = 2 * pi * f

foo x | x < 0 + 5     = "negative"
      | otherwise = "positive"

xdot t [u, y, v, fb, dac, toV, toFb, toDac] = [sU, sY, sV, sFb, sDac, sToV, sToFb, sToDac]
  where
    trf = 1e-9
    fs = 1e6
    (osr, z0, p0, step_dac) = (256, 2*pi*fs, z0/100, 1.0)
    -- fsig = fs / 2 / osr
    fsig = fs / 20
    (sToV, sToFb, sToDac) = (0.0, 0.0, 0.0)
    sU   = dU fsig step_dac t
    sV   = (toV - v) / trf
    sFb  = (toFb - fb) / trf
    sDac = (toDac - dac) / trf
    sY   = (-y + (u - dac) + (sU - sDac) / z0) * p0


-- main = mplot (ts:toColumns (scanl timeStep [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0] ts))
main = do
  let fs = 1e6
  let vs = take 500 $ (!! 0) <$> iterate (last . toLists . timeStep) (replicate 8 0)
  -- let ts = take 500 [0,(1.0/fs)..] 
  let ts = take 500 [0..]
  mplot $ vector ts : [vector vs] 
