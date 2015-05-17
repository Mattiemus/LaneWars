{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Game.Client.Graphics where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Types
    
--
-- Based on code at: 
-- https://github.com/scan/yampa-sdl/blob/master/src/FRP/Yampa/SDL/Graphic.hs
--

-----------
-- Types --
-----------
-- |Alias for an SDL surface
type Screen = SDL.Surface

-- |Type that contains a graphic 
newtype Graphic = Graphic {
        paintGraphic :: Screen -> Int -> Int -> IO () -- ^Function to draw the graphic to the screen
    }

-- |Type for an area mask
data Mask = Mask {
        maskClip :: Maybe Rect -- ^Optional area to max
       ,maskX :: Int -- ^X position of the mask
       ,maskY :: Int -- ^Y position of the mask
    }

-- |Type for an area fill
data Fill = Fill {
        fillClip :: Maybe Rect -- ^Optional area to fill
       ,fillColor :: Color -- ^Color to fill the area with
    }
    
-- |Enumeration of layers that graphics can be placed on
data GraphicLayer = GameLayer Int
                  | UILayer
    deriving(Eq, Ord)
    
-------------------------------
-- Graphical object builders --
-------------------------------
emptyMask :: Mask
emptyMask = Mask Nothing 0 0

-- |Converts a position, width, and height to an SDL rectangle
rectangle :: Vector2D -- ^X and Y position
          -> Int -- ^Width
          -> Int  -- ^Height
          -> SDL.Rect
rectangle pos width height = SDL.Rect x y width height
    where
        x = round (vector2X pos)
        y = round (vector2Y pos)
       
-- |Offsets an SDL rectangle by a specified amount
offsetRect :: Int -- ^X offset
           -> Int -- ^Y offset
           -> SDL.Rect -- ^Rectangle to offset
           -> SDL.Rect
offsetRect x y (SDL.Rect rx ry rw rh) = SDL.Rect (rx - x) (ry - y) rw rh

-------------------------
-- Rendering functions --
-------------------------
-- |Graphic that draws nothing
emptyG :: Graphic
emptyG = Graphic $ \_ _ _ -> return ()

-- |Draws a list of graphics objects
drawAll :: [Graphic] -> Graphic
drawAll [] = emptyG
drawAll (g:gs) = Graphic { 
            paintGraphic = (\s x y -> do
                (paintGraphic g) s x y
                (paintGraphic (drawAll gs)) s x y
                return ())
        }
    
-- |Type class for drawable and maskable graphics
class Draw canvas mask where
    draw :: canvas -> mask -> Graphic
    
-- |Draw instance to expose SDL surfaces to the graphics system
instance Draw SDL.Surface Mask where
    draw src mask = Graphic drawFunc
        where
            drawFunc dst offsetX offsetY = do
                blitSurface src clip dst (offset offsetX offsetY)
                return ()
            clip = maskClip mask
            offset x y = Just (offsetRect x y (Rect (maskX mask) (maskY mask) 0 0))

-- |Draw instance for simple area fills
instance Draw Fill Mask where
    draw fill mask = Graphic drawFunc
        where
            drawFunc dst offsetX offsetY = do
                c <- mapRGB (surfaceGetPixelFormat dst) (colorRed color) (colorGreen color) (colorBlue color)
                fillRect dst (clip offsetX offsetY) c
                return ()
            clip x y = case fillClip fill of
                            Nothing -> Nothing
                            Just rect -> Just (offsetRect x y rect)
            color = fillColor fill