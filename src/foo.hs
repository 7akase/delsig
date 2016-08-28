import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

v = vector [1,2,3]

foo :: Vector Double -> Double
foo = (!! 0) . toList
bar :: Vector Double -> Double
bar (fromList xs) = xs !! 0 
