from numpy import *
from scipy.integrate import odeint
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# DUT model parameter
fs = 1.0e6
ts = 1.0/fs
osr = 256 
z0 = 2*pi*fs
p0 = z0/100
step_dac = 1.0 / 2**8
trf_comp = 1e-8
trf_dac = 1e-8

t_ab = 0.5 * ts
t_ba = 0.5 * ts
sig_jit = 0.002 * ts

# simulation parameter
dt = 1e-8

def U(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        a = step_dac / ts / w # [V] w*A*Ts < step_dac
        if t < ts:
                return 0.0
        else:
                return a * sin(w * t) 

v_next = -1
v_now  = -1
v_prev = -1
fb_next = 0
fb_now  = 0
fb_prev = 0
dac_next = 0.0
dac_now  = 0.0
dac_prev = 0.0
# time = Timer(ts)
def eventAtA(x):
        global v_prev, fb_prev, dac_prev
        global v_now , fb_now , dac_now
        global v_next, fb_next, dac_next
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 

        v_prev = v_next
        v_next = (1 if x[y] > 0 else -1)

        fb_prev = fb_next
        fb_next = v_prev

        dac_prev = dac_next
        dac_next = step_dac * fb_next
        
        v_now = v_next
        fb_now = fb_next
        dac_now = dac_next

        return

def eventAtB(x):
        global v_prev, fb_prev, dac_prev
        global v_now , fb_now , dac_now
        global v_next, fb_next, dac_next
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 

        dac_now = 0.0 # start from zero
        return

def updateState(x, t, t_start, p0, z0):
        global v_now , fb_now , dac_now
        # t_start : start time to calculate U(t)
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 
        # number of signals 
        s = zeros(5)
        
        # update state
        s[v]   = (v_now        - x[v]  ) / dt 
        s[fb]  = (fb_now       - x[fb] ) / dt 
        s[dac] = (dac_now      - x[dac]) / dt 
        s[u]   = (U(t+t_start) - x[u]  ) / dt
        s[y]   = (-x[y] + (x[u] - x[fb])  + (s[u] + s[fb])/z0) * p0 # deriv eq
        
        # differential equation
        # y = (1 + s/z0) / (1 + s/p0) * u
        # -> (1 + s/p0) * Y = (1 + s/z0) * u
        # -> s*y = (-y + (1 + s/z0)u) * p0
        # -> s*y = (-y + u + s*u/z0) * p0
        ## s[u]  = (U(t) - x[u]) / dt
        return  s

def runClock(dut, t_start, init_cond):
        global ts, dt, sig_jit
        global p0, z0

        t1 = arange(0, t_ab + random.normal(0, sig_jit), dt)
        t2 = arange(0, t_ba + random.normal(0, sig_jit), dt)
        t2 = map(lambda x: x + t1[-1], t2)
        
        state_half = odeint(dut, init_cond,      t1, args=(t_start, p0, z0))
        eventAtA(state_half[-1,:]) # comparator / DAC
        state_full = odeint(dut, state_half[-1], t2, args=(t_start, p0, z0))
        eventAtB(state_full[-1,:]) # DAC (RZ)

        t     = concatenate((t1,         t2        ))
        state = concatenate((state_half, state_full))
        return (t, state)

t = []; u = []; y = []; v = []; fb = []; 
state = zeros((1,5))
t_start = 0
tt = [0]
for i in range(0, osr*5):
        t_start += tt[-1]
        tt, state = runClock(updateState, t_start, state[-1,:])
        t.extend(map(lambda x: x + t_start, tt))
        u.extend(state[:,0])
        y.extend(state[:,1])
        v.extend(state[:,2])
        fb.extend(state[:,3])

fig, (sp1, sp2) = plt.subplots(nrows=2, figsize=(10,7))
sp1.plot(t, u,  label = "U" )
sp2.plot(t, y,  label = "Y" )
sp2.plot(t, v,  label = "V" )
sp1.plot(t, fb, label = "FB")
p1.legend()
sp2.legend()
plt.show()
