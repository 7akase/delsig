from numpy import *
from numpy.fft import *
from scipy import *
import matplotlib.pyplot as plt

def db10(x):
        return 10 * log10(x)

def db20(x):
        return 20 * log10(x)

def psd(ps):
        return db10(abs(fft(ps)))

def hann(xs):
        n = len(xs)
        ws = [0.5 - 0.5 * cos(2*pi*x) for x in arange(0,n)/float(n-1)]
        return [w * x for w, x in zip(ws, xs)]

def blackman(xs):
        n = len(xs)
        ws = [0.42 - 0.5*cos(2*pi*x) + 0.08*cos(4*pi*x) for x in arange(0,n)/float(n-1)]
        return [w * x for w, x in zip(ws, xs)]

def v2p(vs):
        return array(vs) ** 2

dt = 1.0
n_fft = 2**5
xs = [sin(2*pi*x) for x in arange(0,n_fft)/float(n_fft)]
df = 1 / dt / len(xs)
fs = [n * df for n in range(0,len(xs))]
ys = db10(abs(fft(hann(v2p(xs)))))
plt.figure(1)
plt.plot(fs, xs)
plt.show()
