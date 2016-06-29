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

xdot t [x,v] = [v, -0.95*x-0.1*v]
ts = linspace 100 (0,20)
sol = odeSolve xdot [10,0] ts
dt = (toList ts) !! 1

next :: Time -> (Time, [Voltage]) -> (Time, [Voltage])
next dt (t, ini) = (t', sol)
  where
    t' = t + dt
    sol = last . toLists $ odeSolve xdot ini (vector [t, t'])

main = plotLists [] . transpose . fmap snd . take 100 $ iterate (next dt) (0, [10,0])
-- main = plotLists [] (ts : toColumns sol)
