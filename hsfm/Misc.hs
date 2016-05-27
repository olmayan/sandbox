module Misc where

import System.Glib.GDateTime
import System.Time -- should be replaced with Data.Time

integralToDouble :: Integral a => a -> Double
integralToDouble v = fromIntegral v :: Double

gTimeValToClockTime :: GTimeVal -> ClockTime
gTimeValToClockTime GTimeVal {gTimeValSec  = seconds
                             ,gTimeValUSec = microseconds} =
    TOD (toInteger seconds) (toInteger microseconds * 1000)
