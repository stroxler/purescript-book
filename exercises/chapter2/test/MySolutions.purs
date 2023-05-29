module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (pi)
import Data.Number as Data.Number




diagonal :: Number -> Number -> Number
diagonal a b = Data.Number.sqrt (a * a + b * b)


circleArea :: Number -> Number
circleArea r = pi * r * r


leftoverCents :: Int -> Int
leftoverCents original = rem original 100