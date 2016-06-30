import Data.Random
import Control.Monad
import Control.Applicative
import Text.Printf
import Data.Random.Normal

mean = 1.0
stdev = 2.0
seed = 1

get_random :: [Double]
get_random = mkNormals' (mean, stdev) seed

n = 1000 :: Int

rs = take n $ get_random
mean_sim  = sum rs / fromIntegral (length rs)
stdev_sim = sqrt . (/ fromIntegral n) . sum $ fmap (\x -> (x - mean_sim) ** 2) rs
