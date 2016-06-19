
runge dt f y0 = runge'
  where
    runge' = y0 : zipWith runge'' runge' [dt * x | x <- [1..]]
      where
        runge'' x t = x + (k1 + 2*k2 + 2*k3 + k4) / 6
          where
            k1 = dt * f x t
            k2 = dt * f (x + k1/2) (t + dt/2)
            k3 = dt * f (x + k2/2) (t + dt/2)
            k4 = dt * f (x + k3)   (t + dt)

f x t = x
