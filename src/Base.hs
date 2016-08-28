module Base where

import Control.Applicative
import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

import Text.Printf

type Voltage     = Double
type Current     = Double
type Power       = Double
type Res         = Double
type Cap         = Double
type Ind         = Double
type Time        = Double
type Freq        = Double
type AngularFreq = Double

pick :: [Int] -> [[a]] -> [[a]]
pick rows xss = fmap (pickRows rows) xss
  where
    pickRows rows xs = fmap (xs !!) rows

choose :: [Int] -> [[a]] -> [[a]]
choose cols xss = fmap (xss !!) cols

showRecord :: [Double] -> String
showRecord = concat . fmap (printf "%10.2e")

newtype Rec a = Rec [[a]]

joint :: String -> [String] -> String
joint = intercalate

printRec :: Show a => Rec a -> String
printRec (Rec xss) = joint "\n" $ fmap (joint "," . fmap show) xss
