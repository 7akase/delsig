{-# LANGUAGE TemplateHaskell #-}
module Delsig where

import Data.List
import Numeric.LinearAlgebra.Data
import Numeric.GSL.ODE

import Control.Arrow
import Control.Lens

-- | # Dimensinos 
type Time       = Double
type Freq       = Double
type AngularFreq = Double

type Voltage    = Double
type Current    = Double

type SignalSource  = Time -> Voltage

type Osr       = Double
type Bit       = Int

-- | # Modulator
data Modulator = Modulator {
                   _fs :: Freq,
                   _osr :: Osr,
                   _n_dac :: Bit
                 } deriving (Show)
$(makeLenses ''Modulator)

-- data StateVector = StateVector{

m = Modulator{ _fs = 1e0, _osr = 256, _n_dac = 8}

-- | # Simulator

run :: SignalSource -> (Time, [Voltage]) -> [Time] -> [(Time, [Voltage])]

run :: (Time, [Voltage]) -> [Time] -> [(Time, [Voltage])]
run = scanl (flip runOneTimeSlot)

runOneTimeSlot :: Time -> (Time, [Voltage]) -> (Time, [Voltage])
-- | 
--
runOneTimeSlot dt = (id *** runDt) . runCt dt

runDt :: [Voltage] -> [Voltage]
runDt [u, zv, zzv, zfb, x, y] = [u, v, zv, fb, x', y]
  where
    v     = if y >= 0 then 1 else -1
    fb    = zfb + (2 * v - zv) / 2**n_dac
    x'    = (u - fb) / 2
    n_dac = 8

runCt :: Time -> (Time, [Voltage]) -> (Time, [Voltage])
runCt dt (t, vs) = (t', vs')
  where 
    t' = t + dt
    vs' = last . toLists . odeSolve xdot vs $ linspace 2 (t, t')
    xdot t [u, v, zv, fb, x, y] = [su, sv, szv, sfb, sx, sy]
      where
        su  = f' t
        sv  = 0
        szv = 0
        sfb = 0
        sx  = (su - sfb) / 2
        sy  = wp0 * (x - y + sx / wz0)
        wp0 = 2 * pi * 1e0 / 100 :: AngularFreq
        wz0 = 2 * pi * 1e0 / 2 :: AngularFreq


-- f t = 1.0 / 256 * (_osr
f' :: Time -> Voltage
f' = id 
