module Base where

import Control.Applicative
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

import Text.Printf
{-
putStrLn . unlines $ concat <$> fmap (printf "%0.2e, ") <$> log
-}

pick :: [Int] -> [[a]] -> [[a]]
pick rows xss = fmap (pickRows rows) xss
  where
    pickRows rows xs = fmap (xs !!) rows

showRecord :: [Double] -> String
showRecord = concat . fmap (printf "%10.2e")
