from numpy import *
from scipy.integrate import *

y0, t0 = [1.0, 1.0], 0

def f(t, y, trf, tau):
        # Y/X = 1/(1+s*tau)
        # <=> Y + s*tau*Y = X <=> sY = (X-Y)/tau
        return [(0-y[0])/trf, (0 - y[1])/tau]

r = ode(f).set_integrator('dorpi5', with_jacobian=False)

r.set_initial_value(y0, t0).set_f_params(0.1, 1.0)

t1 = 10
dt = 1
while r.successful() and r.t < t1:
        r.integrate(r.t+dt)
        print("%g %g %g" %(r.t, r.y[0], r.y[1]))
