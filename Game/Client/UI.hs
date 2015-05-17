module Game.Client.UI where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Types
import Game.Client.Graphics

-- |Generates a graphic object that will draw a progress bar
drawProgressBar :: Rect -- ^Area in which the progress bar will be drawn
                -> SDL.Color -- ^Foreground color
                -> SDL.Color -- ^Background color
                -> Int -- ^Border width
                -> Double -- ^Progress bar percentage
                -> Graphic
drawProgressBar rect bgColor fgColor border percent = 
    let clampedPercent = min 1.0 (max percent 0.0)
        foreGroundRect = Rect ((rectX rect) + border)
                              ((rectY rect) + border)
                              (round ((fromIntegral ((rectW rect) - (border + border))) * clampedPercent))
                              ((rectH rect) - (border + border)) in
    drawAll [draw (Fill (Just rect) bgColor) emptyMask,
             draw (Fill (Just foreGroundRect) fgColor) emptyMask]