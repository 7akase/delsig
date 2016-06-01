from init import *
from numpy import *

f0 = 1e6 # Hz
osr = 256 
fb = f0 / osr # Hz

def dfdt_max(f0, n):
        ita = 1.0
        return 2 * n * ita * f0

def sqISF(dfdt_max):
        u"""rms of the squared impusle sensitivity function [sec^3]

        """
        return 2.0 / 3.0 / pi / dfdt_max ** 3


def phase_noise(f0, p):
        u""" return function of phase noise at 'df' offset frequency [1/Hz] (which becomes [dBc/Hz] after db10)
        
        f0    : oscilaltion frequency [Hz]
        power : inverter chain power consumption [W]
        df    : offset frequency from f [Hz]
        """
        gamma = 2.0 / 3
        ita = 0.75
        return lambda(df) : (16 * gamma / 3 / ita) * k * T / p * f0**2 / df**2

def kappa(p):
        ita = 0.75
        gamma = 2.0 / 3.0
        return sqrt(16.0 * gamma / 3.0 / ita * k * T / p) # [sec]

def sig_jitter(f0, power):
        return kappa(power) * sqrt(1.0 / f0) # [sec]


def var_jitter_per_cycle(f0, l, df):
        u""" see Eq.(50) and (51) in ref[0]
        
        But these equations treats phase noise L(df) in dB.
        """
        return df / f0 ** 1.5 * sqrt(l(df)) # [sec]

p = 1e-6
print(kappa(1e-6) * sqrt(1.0 / f0))
print(var_jitter_per_cycle(f0, phase_noise(f0, p), 1.0/f0))
print("{0:.1f}ps @ 1MHz, 1uW".format(sig_jitter(f0, p) / 1e-12))

def reference():
        refs = [["A. Hajimiri, S. Limotyrakis and T. H. Lee" \
                ,"Jitter and Phase Noise in Ring Oscillators" \
                ,"IEEE J. Solid-State Circuits, vol. 34, pp. 790-804 , June, 1999" \
                ]
                ]
        return refs

