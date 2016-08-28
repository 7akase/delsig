-- module Mod where

import Numeric.GSL.ODE
import Numeric.LinearAlgebra.Data
import Graphics.Gnuplot.Simple
import Data.List

type Time    = Double
type Voltage = Double
type Current = Double
type Res     = Double
type Cap     = Double
type Ind     = Double
type Freq    = Double

ts = linspace 100 (0,20)
dt = (toList ts) !! 1

fs = 1 / dt
f_sig = 10.0 
w_sig = 2*pi*f_sig

dfdt :: Time -> Voltage
dfdt t = 0.7 * w_sig * cos (w_sig*t)

-- ==========================================================
-- state : V, X1, X2, U
--
ini :: (Time, [Double])
ini = (0, [0,0,0,0])

genXdot :: (Time -> Voltage) -> (Time -> [Voltage] -> [Voltage])
genXdot dfdt = xdot
  where
    xdot t [v, x1, x2, u] = [sv, sx1, sx2, su]
      where
        sv  = 0
        sx1 = (u  - v) 
        sx2 = (x1 - v)
        su  = dfdt t

runMod :: (Time -> Voltage) -> Time
            -> (Time, [Voltage]) -- initial condition
            -> [(Time, [Voltage])] -- result
runMod dfdt dt = iterate $ runDt . (runCt dfdt dt)

runDt :: (Time, [Voltage]) -> (Time, [Voltage])
runDt (t, [zv, x1, x2, u]) = (t, [q x2, x1, x2, u])
  where
    q x = if x >= 0 then 1 else -1

-- ==========================================================
runCt :: (Time -> Voltage) -> Time -> (Time, [Voltage]) -> (Time, [Voltage])
runCt  dfdt dt (t, ini) = (t', sol)
  where
    t' = t + dt
    sol = last . toLists $ odeSolve xdot ini (vector [t, t'])
    xdot = genXdot dfdt 

main = plotLists [] . transpose . fmap snd . take 100 $ runMod dfdt dt ini
