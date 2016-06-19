import Control.Applicative
import Data.Complex
import Numeric.FFT
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Graphics.Plot
sample :: IO()
sample = do
  mplot $ vector <$> [0..255] : (fmap ((2/256*) . realPart . abs) $ fft $ (:+0) <$> sin . (2*pi/8*) <$> [0..255]) : []

