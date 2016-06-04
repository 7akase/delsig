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
t = arange(0, ts*osr*5, dt)

class Timer:
        def __init__(self, ts):
                self.ts = ts 
                self.clk_count = 0
                self.prev_time = 0.0

        def isInRange(self, a, b, t):
                tt = mod(t, self.ts)
                if tt >= a and tt < b:
                        return True
                else:
                        return False
        
        def isPosedge(self, t):
                if int(t / self.ts) > self.clk_count:
                        self.clk_count = int(t / self.ts)
                        self.prev_time = t
                        return True
                else:
                        return False


def U(t):
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

def mod_ts(t):
        global clk_count
        global ts
        clk_count += 1 if t > ts * clk_count else 0 
        return t / ts - (clk_count - 1)

v_next = -1
v_prev = -1
fb_next = 0
fb_prev = 0
dac_next = 0.0
dac_prev = 0.0
time = Timer(ts)
def updateState(x, t, t_start, p0, z0):
        # t_start : start time to calculate U(t)
        global v_next, v_prev
        global fb_next, fb_prev
        global dac_next, dac_prev
        global timer
        # index definition
        u = 0
        y = 1
        v = 2
        fb = 3
        dac = 4
        # number of signals 
        s = zeros(5)
        

        alpha = 0.0
        beta = 1.0
        
        # comparator
        if time.isPosedge(t):
                v_prev = v_next
                v_next = (1 if x[y] > 0 else -1)

                fb_prev = fb_next
                fb_next = v_prev

                dac_prev = dac_next
                dac_next = step_dac * fb_next
        else:
                pass
        
        # comparator
        v_now = v_next

        # feedback filter
        fb_now = fb_next # 

        # RZ-DAC response
        if time.isInRange(0.0, alpha, t):
                dac_now = 0.0 # start from zero
        elif time.isInRange(alpha, beta, t):
                dac_now = dac_next
        else:
                dac_now = 0.0 # return to zero
        
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
        global ts, dt
        global p0, z0
        t = arange(0, ts, dt)
        ret = odeint(dut, init_cond, t, args=(t_start, p0, z0))
        return (t, ret)

t = []
u = []
y = []
v = []
fb = []
state = zeros((1,5))
t_start = 0
for i in range(0, osr*5):
        t_start += tt[-1] + dt
        time.clk_count = 0
        tt, state = runClock(updateState, t_start, state[-1,:])
        t.extend(map(lambda x: x + t_start, tt))
        u.extend(state[:,0])
        y.extend(state[:,1])
        v.extend(state[:,2])
        fb.extend(state[:,3])

# vv = odeint(updateState, zeros(5), t, args=(p0, z0))
fig, (sp1, sp2) = plt.subplots(nrows=2, figsize=(10,7))
sp1.plot(t, u,  label = "U" )
sp2.plot(t, y,  label = "Y" )
sp2.plot(t, v,  label = "V" )
sp1.plot(t, fb, label = "FB")
# sp1.plot(t, vv[:,0], label = "U")
# sp2.plot(t, vv[:,1], label = "Y")
# sp2.plot(t, vv[:,2], label = "V")
# sp1.plot(t, vv[:,3], label = "FB")
sp1.legend()
sp2.legend()
plt.show()
