module Base where

import Control.Applicative
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

import Text.Printf

pick :: [Int] -> [[a]] -> [[a]]
pick rows xss = fmap (pickRows rows) xss
  where
    pickRows rows xs = fmap (xs !!) rows

choose :: [Int] -> [[a]] -> [[a]]
choose cols xss = fmap (xss !!) cols

showRecord :: [Double] -> String
showRecord = concat . fmap (printf "%10.2e")
