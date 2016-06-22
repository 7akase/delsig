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

xdot :: (Double -> Double) -> Double -> [Double] -> [Double]
xdot u' t [u , y , v ,  fb,  dac, toV, toFb, toDac, startTime]
     = [sU, sY, sV, sFb, sDac,   0,    0,     0,         0]
  where
    sU   = u' (t + startTime)
    sV   = (toV - v) / trf
    sFb  = (toFb - fb) / trf
    sDac = (toDac - dac) / trf
    sY   = (-y + (u - dac)) * p0
    -- sY   = (-y + (u - dac) + (sU - sDac) / z0) * p0

eventAtAlpha [u, y, v, fb, dac, toV,  toFb,  toDac,  start_time]
           = [u, y, v, fb, dac, toV', toFb', toDac', start_time']
  where
    toV'   = adc y
    toFb'  = toV'
    toDac' = step_dac * toFb'
    step_dac = 1.0
    start_time' = start_time + 1.0/fs

adc x | x >  0 = 1
      | x <= 0 = -1

xdotV :: (Double -> Double) -> Double -> Vector Double -> Vector Double
xdotV u' t vec = vector $ xdot u' t (toList vec) 

