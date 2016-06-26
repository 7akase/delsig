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
fs = 1.0
osr = 256 :: Int
(step_dac) = (1.0)

-- delta sigma modulator definition
type DutVec = [Double]
type TIME = Double
type DU   = Double
type U    = Double
type Y    = Double -- comparator input
type ZY   = Double --
type V    = Double -- 
type DAC  = Double

h :: U -> DAC -> ZY -> Y
-- | forward-path filter
h = undefined 

q :: Y -> V
-- | quantizer
q x | x >  0 = 1
    | x <= 0 = -1

g :: V -> DAC
-- | feedback-path filter and dac
g = id

-- one-timestep
next :: (TIME -> U) -> DutVec -> DutVec
next f ve_prev = [time, v, u, y, dac]
  where
    [time', _, u', y', dac'] = ve_prev
    u    = f time'
    y    = u' - dac' + y'
    v    = q y
    dac  = g v
    time = time' + 1.0/fs 
