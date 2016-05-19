import Control.Applicative
import Text.Printf
import Text.CSV as Csv



vsig = 1.0
osr = 256
fb = 1e3
fs = 1e6
delta = 1.0

sigJitter = 1e-12
sigNTF = 1/100

ibjn1 = 2 * pi * (vsig * fb * sigJitter) ** 2 / osr

ibjn2 = osr * (fb * delta * sigNTF * sigJitter) ** 2 / 3

log10 :: Floating a => a -> a
log10 x = log x / log 10 

db10 :: Floating a => a -> a
db10 = (10 *) . log10

db20 :: Floating a => a -> a
db20 = (20 *) . log10

