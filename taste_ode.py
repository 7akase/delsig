# from numpy import *
from scipy.integrate import *

y0, t0 = [1.0, 2.0], 0

def f(t, y, arg1):
        return [arg1*y[0] + y[1], -arg1*y[1]**2]

def jac(t, y, arg1):
        return [[arg1, 1], [0, -arg1*2*y[1]]]

r = ode(f).set_integrator('dorpi5', with_jacobian=False)

r.set_initial_value(y0, t0).set_f_params(2.0)

t1 = 10
dt = 1
while r.successful() and r.t < t1:
        r.integrate(r.t+dt)
        print("%g %g" %(r.t, abs(r.y[0])))
