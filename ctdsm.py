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

t_ab = 0.9 * ts # RZ DAC
t_ba = 0.1 * ts # RZ DAC
t_ab = 1.0 * ts # NRZ DAC
t_ba = 0.0 * ts # NRZ DAC
sig_jit = 0.01 * ts

# simulation parameter
dt = 1e-8

def U_DM(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        if t < ts:
                return 0.0
        else:
                return (step_dac / ts / w) * sin(w * t) 

def U_DSM(t):
        fsig = fs / 2 / osr # [Hz]
        w = 2*pi*fsig # [rad/sec]
        if t < ts:
                return 0.0
        else:
                return step_dac * sin(w * t) 

U = U_DSM

u = 0; y = 1; v = 2; fb = 3; dac = 4; 
to = zeros(5)
def eventAtA(x):
        global to 
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 

        to[v]   = (1 if x[y] > 0 else -1)
        to[fb]  = to[v] 
        to[dac] = step_dac * to[fb]
        return

def eventAtB(x):
        global to
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 

        to[dac] = 0.0 # start from zero
        return

def updateState(x, t, t_start, p0, z0):
        global to
        u = 0; y = 1; v = 2; fb = 3; dac = 4; 
        s = zeros(5)
        
        # update state
        s[v]   = (to[v]         - x[v]  ) / dt 
        s[fb]  = (to[fb]        - x[fb] ) / dt 
        s[dac] = (to[dac]       - x[dac]) / dt 
        s[u]   = (U(t+t_start)  - x[u]  ) / dt
        s[y]   = (-x[y] + (x[u] - x[dac])  + (s[u] - s[dac])/z0) * p0 # deriv eq
        
        # differential equation
        # y = (1 + s/z0) / (1 + s/p0) * u
        # -> s*y = (-y + u + s*u/z0) * p0 , u = u - dac
        return  s

def runClock(dut, t_start, init_cond):
        global ts, dt, sig_jit
        global p0, z0
        global t_ab, t_ba

        alpha = random.normal(0, sig_jit)
        beta  = random.normal(0, sig_jit)
        print (alpha, beta) 
        t1 = arange(0, t_ab + alpha, dt)
        t2 = arange(0, t_ba + beta,  dt)

        concatenate((t1, [t_ab + alpha])) # 0 ~ t_ab + jitter
        concatenate((t2, [t_ba + beta ])) # 0 ~ t_ba + jitter
        t2 = map(lambda x: x + t1[-1], t2) # 0 ~ t_ab+jitter, t_ab+jitter ~ (t_ab+t_ba+2*jitter)
        
        state_half = odeint(dut, init_cond,      t1, args=(t_start, p0, z0))
        eventAtA(state_half[-1,:]) # comparator / DAC
        
        if(t_ba > dt): # when t_ba = 0 sec (NRZ DAC)
                state_full = odeint(dut, state_half[-1], t2, args=(t_start, p0, z0))
                eventAtB(state_full[-1,:]) # DAC (RZ)
                t     = concatenate((t1,         t2        ))
                state = concatenate((state_half, state_full))
        else:
                t     = t1 
                state = state_half 

        return (t, state)

t = []; u = []; y = []; v = []; fb = []; dac = []
state = zeros((1,5))
t_start = 0
tt = [0]
for i in range(0, osr*5):
        t_start += tt[-1];
        tt, state = runClock(updateState, t_start, state[-1,:])
        t.extend(map(lambda x: x + t_start, tt))
        u.extend(state[:,0])
        y.extend(state[:,1])
        v.extend(state[:,2])
        fb.extend(state[:,3])
        dac.extend(state[:,4])

fig, (sp1, sp2) = plt.subplots(nrows=2, figsize=(10,7))
sp1.plot(t, u,  label = "U" )
sp2.plot(t, y,  label = "Y" )
sp2.plot(t, v,  label = "V" )
sp1.plot(t, fb, label = "FB")
sp1.legend()
sp2.legend()
plt.show()
