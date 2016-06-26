module Dsm1 where

import Base
import Psd
import Delsig

import Control.Applicative
import Data.List

import Numeric.GSL
import Numeric.GSL.ODE
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

-- xdot t [x,v] = [v, -0.95*x-0.1*v]
fs = 1e6
trf = 1e-9
osr = 256 :: Int
(z0, p0, step_dac) = (2*pi*fs, z0/100, 1.0)

-- xdotV :: [Double] -> (Double -> Double) -> Double -> Vector Double -> Vector Double
-- xdotV to_state u' ini t vec = vector $ xdot to_state u' t (toList vec) 

xdotV :: (Double -> Double) -> Double -> VecEle -> VecEle 
xdotV u' t ve
  = vector [sU, sY, sV, sFb, sDac, 0, 0, 0, 0] 
  where
    [u, y, v, fb, dac, toV, toFb, toDac, start_time] = toList ve
    sU   = u' (t + start_time)
    sV   = (toV - v) / trf
    sFb  = (toFb - fb) / trf
    sDac = (toDac - dac) / trf
    sY   = (-y + (u - dac)) * p0
    -- sY   = (-y + (u - dac) + (sU - sDac) / z0) * p0

eventAtAlpha :: VecEle -> VecEle 
eventAtAlpha ve_prev
  = vector [u, y, v, fb, dac, toV', toFb', toDac', start_time']
  where
    [u, y, v, fb, dac, toV,  toFb , toDac , start_time] = toList ve_prev
    toV'   = adc y
    toFb'  = toV'
    toDac' = step_dac * toFb'
    step_dac = 1.0
    start_time' = start_time + 1.0/fs

adc x | x >  0 = 1
      | x <= 0 = -1


