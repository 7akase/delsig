from numpy import *
from scipy.integrate import odeint
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


fs = 1.0e6
ts = 1.0/fs
osr = 256 
z0 = 2*pi*fs
p0 = z0/100
step_dac = 1.0 / 2**8
trf_comp = 1e-8
trf_dac = 1e-8

dt = 1e-8
t = arange(0, ts*osr*2, dt)


def next_u(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        a = step_dac / ts / w # [V] w*A*Ts < step_dac
        if t < ts:
                return 0.0
        else:
                return a * sin(w * t) 

clk_count = 0
def isClock(t):
        global clk_count
        global ts
        if t > ts * clk_count :
                print clk_count
                clk_count += 1
                return True
        else:
                return False

v_now = -1
v_prev = -1
fb_now = 0
fb_prev = 0
def foo(x, t, p0, z0):
        global v_now, v_prev
        global fb_now, fb_prev
        # index definition
        u = 0
        y = 1
        v = 2
        fb = 3
        # number of signals 
        s = zeros(4)
        
        # comparator
        if isClock(t):
                v_prev = v_now
                v_now  = (1 if x[y] > 0 else -1)

                fb_prev = fb_now
                fb_now = v_now + (fb_prev + v_now)
        else:
                pass
        s[v] = (v_now - x[v]) / trf_comp
        
        
        # DAC output
        s[fb] = -step_dac * v_now / trf_dac # negative feedback

        # differential equation
        # y = (1 + s/z0) / (1 + s/p0) * u
        # -> (1 + s/p0) * Y = (1 + s/z0) * u
        # -> s*y = (-y + (1 + s/z0)u) * p0
        # -> s*y = (-y + u + s*u/z0) * p0
        s[u]  = (next_u(t) - x[u]) / dt
        s[y]  = (-x[y] + (x[u] + x[fb])  + (s[u] + s[fb])/z0) * p0
        return  s

vv = odeint(foo, zeros(4), t, args=(p0, z0))
fig, (sp1, sp2) = plt.subplots(nrows=2, figsize=(10,7))
sp1.plot(t, vv[:,0], label = "U")
sp2.plot(t, vv[:,1], label = "Y")
sp2.plot(t, vv[:,2], label = "V")
sp1.plot(t, vv[:,3], label = "FB")
sp1.legend()
sp2.legend()
plt.show()
