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
-- xdot t [x,v] = [v, -0.95*x-0.1*v]
-- sol = odeSolve xdot [10,0] ts
dt = (toList ts) !! 1

fs = 1 / dt
f_sig = fs / 23
w_sig = 2*pi*f_sig

dfdt :: Time -> Voltage
dfdt t = 0.7 * w_sig * cos (w_sig*t)

genXdot :: (Time -> Voltage) -> (Time -> [Voltage] -> [Voltage])
genXdot dfdt = xdot
  where
    xdot t [x,v,u] = [v, -0.95*x-0.1*v, dfdt t]

next :: (Time -> Voltage) -> Time -> (Time, [Voltage]) -> (Time, [Voltage])
next dfdt dt (t, ini) = (t', sol)
  where
    t' = t + dt
    sol = last . toLists $ odeSolve xdot ini (vector [t, t'])
    xdot = genXdot dfdt 

runCtMod :: (Time -> Voltage) -> Time
            -> (Time, [Voltage]) -- initial condition
            -> [(Time, [Voltage])] -- result
runCtMod dfdt dt = iterate (next dfdt dt)

main = plotLists [] . transpose . fmap snd . take 100 $ iterate (next dfdt dt) (0, [10,0,0])

main2 = plotLists [] . transpose . fmap snd . take 100 $ runCtMod dfdt dt (0, [10,0,0])
