from numpy import *
from scipy.integrate import * 
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# DUT model parameter
fs = 1.0e6
ts = 1.0/fs
osr = 256 
z0 = 2*pi*fs
p0 = z0/100
# step_dac = 1.0 / 2**8
step_dac = 1.0
trf_comp = 1e-7
trf_dac = 1e-7

# t_ab = 0.9 * ts # RZ DAC
# t_ba = 0.1 * ts # RZ DAC
t_ab = 1.0 * ts # NRZ DAC
t_ba = 0.0 * ts # NRZ DAC
sig_jit = 0.01 * ts

def dU_DM(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        if t < ts:
                return 0.0
        else:
                return (step_dac / ts / w) * w * cos(w * t) 

def dU_DSM(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        if t < ts:
                return 0.0
        else:
                return step_dac * w * cos(w * t) 

dU = dU_DSM

def eventAtA(x):
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 
        to = x
        to[v]   = (1 if x[y] > 0 else -1)
        to[fb]  = to[v] 
        to[dac] = step_dac * to[fb]
        return to

def eventAtB(x):
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 
        to = x
        to[dac] = 0.0 # start from zero
        return to

def updateState(t, x, p0, z0, to):
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 
        s = zeros(5)
        
        # update state
        s[v]   = (to[v]   - x[v]  ) / trf_comp 
        s[fb]  = (to[fb]  - x[fb] ) / trf_comp
        s[dac] = (to[dac] - x[dac]) / trf_dac
        s[u]   = dU(t) 
        s[y]   = (-x[y] + (x[u] - x[dac])  + (s[u] - s[dac])/z0) * p0 # deriv eq
        
        # differential equation
        # y = (1 + s/z0) / (1 + s/p0) * u
        # -> s*y = (-y + u + s*u/z0) * p0 , u = u - dac
        return  s

u = 0; y = 1; v = 2; fb = 3; dac = 4; 
t = [0];
ys = zeros((5,1)) 
t_start = 0
t_end   = ts * osr * 2 * 3
r = ode(updateState).set_integrator('dorpi5', with_jacobian=False)
r.set_initial_value(zeros(5), t_start).set_f_params(p0, z0, zeros(5))
while r.successful() and r.t < t_end:
        alpha = random.normal(0, sig_jit)
        beta  = random.normal(0, sig_jit)
        r.set_initial_value(r.y, r.t).set_f_params(p0,z0, eventAtA(r.y))
        r.integrate(r.t + t_ab + alpha)
        
        t.append(r.t)
        ys = c_[ys, r.y]

        r.set_initial_value(r.y, r.t).set_f_params(p0,z0, eventAtB(r.y))
        r.integrate(r.t + t_ba + beta)
        # print("%g : %g %g %g" % (r.t, r.y[dac], r.y[y], r.y[v]))

stats = ys
us   = stats[0,:]
ys   = stats[1,:]
vs   = stats[2,:]
fbs  = stats[3,:]
dacs = stats[4,:]

fig, (sp1, sp2) = plt.subplots(nrows=2, figsize=(10,7))
sp1.plot(t, us,   label = "U" )
sp1.plot(t, dacs, label = "DAC")
sp2.plot(t, ys,   label = "Y" )
sp2.plot(t, vs,   label = "V" )
# sp1.plot(t, fbs, label = "FB")
sp1.legend()
sp2.legend()
plt.show()
