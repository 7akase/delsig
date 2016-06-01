from init import *
from numpy import *

pi2 = 2.0 * pi

fs = 1.0e6
osr = 256
fB = fs / 2 / osr

vdd = 1.0
n_dac = 8
step_comp = vdd
step_dac = vdd / 2**n_dac

v_NTF = 1.0

v_jitter = (100.0e-12) ** 2 # clock jitter [sec]

dr = step_dac * osr / pi # input full-scale [V]

v_aperture = 0.0
v_ibjn1 = 2 * pi**2 * dr**2 * fB ** 2 * v_jitter / osr
v_ibjn2 = osr * fB**2 * step_comp**2 * v_NTF * v_jitter / 3

v_ibjn = v_ibjn1 + v_ibjn2

snr = (dr**2 / 2) / v_ibjn

print("DR     = {0:.2f}V".format(dr))
print("jitter = {0:.1f}ps".format(sqrt(v_jitter) / 1e-12))
print("SNR    = {0:.1f}dB".format(db10(snr)))

print("* var NTF is not taken into consideration")
