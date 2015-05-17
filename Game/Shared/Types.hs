module Game.Shared.Types where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Data.Serialize
import Data.Ord

--------------------
-- Geometry types --
--------------------
-- |Alias for a 2-dimensional vector of double-precision floating point values
type Vector2D = Vector2 Double

-- |Computes the distance between two vectors
vectorDistance :: RealFloat a => Vector2 a -> Vector2 a -> a
vectorDistance a b = norm (a ^-^ b)
        
-- |Returns the components of a vector as a tuple
vectorComponents :: RealFloat a => Vector2 a -> (a, a)
vectorComponents x = (vector2X x, vector2Y x)

-- |First rounds the components of a vector to the nearest integer value, 
-- then returns the vector as a tuple
vectorRoundedComponents :: (RealFloat a, Integral b) => Vector2 a -> (b, b)
vectorRoundedComponents x = (round (vector2X x), round (vector2Y x))

instance (RealFloat a, Serialize a) => Serialize (Vector2 a) where
    put vec = do
        put (vector2X vec)
        put (vector2Y vec)
    get = do
        x <- get
        y <- get
        return (vector2 x y)
       
-- |Performs linear interpolation on a vector
lerpV :: RealFloat a => Vector2 a -- ^Start value
      -> Vector2 a -- ^End value
      -> a -- ^Percentage along the interpolation path
      -> Vector2 a -- ^Interpolated value
lerpV start end percent = start ^+^ (percent *^ (end ^-^ start))