module Game.Shared.Physics where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

-- |Predicate that tests (inclusively) if a value is within a specific range
withinRange :: Double -- ^Value to test
            -> Double -- ^Maximum value
            -> Double -- ^Minimum value
            -> Bool -- ^True if the value is within the minimum and maximum
withinRange val min max = (val >= min) && (val <= max)
    