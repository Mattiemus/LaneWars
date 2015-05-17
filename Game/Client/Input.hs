{-# LANGUAGE Arrows #-}

module Game.Client.Input where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Types

--
-- Based on code at: 
-- https://github.com/scan/yampa-sdl/blob/master/src/FRP/Yampa/SDL/Event.hs
--

-----------------
-- Event types --
-----------------
-- |Type alias for a list of SDL input events
type PlayerInput = [SDL.Event]

---------------------------
-- Keyboard input events --
---------------------------
-- |Tests if an SDL keyboard input is that of a specific key
isKey k (SDL.Keysym ks _ _) | k == ks = True
isKey _ _ = False

-- |Signal function returning the list of keys that are currently pressed
keysDown :: SF PlayerInput [SDL.Keysym]
keysDown = arr $ map fromKeyDown . filter isKeyDownEvent
    where
        fromKeyDown (SDL.KeyDown k) = k
        isKeyDownEvent (SDL.KeyDown _) = True
        isKeyDownEvent _ = False

-- |Signal function returning an event that fires when a specific key is pressed
keyDown :: SDLKey -- ^Key to test
        -> SF PlayerInput (Yampa.Event ())
keyDown k = edge <<< arr (not . null . filter (isKey k)) <<< keysDown

-- |Signal function returning the list of keys that are currently released
keysUp :: SF PlayerInput [SDL.Keysym]
keysUp = arr (map fromKeyUp . filter isKeyUpEvent)
    where
        fromKeyUp (SDL.KeyUp k) = k
        isKeyUpEvent (SDL.KeyUp _) = True
        isKeyUpEvent _ = False

-- |Signal function returning an event that fires when a specific key is pressed
keyUp :: SDLKey -- ^Key to test
       -> SF PlayerInput (Yampa.Event ())
keyUp k = edge <<< arr (not . null . filter (isKey k)) <<< keysUp

-- |Switches between two values depending on if a key is pressed or not
keyPressSwitch :: SDLKey -- ^Key to bind value to
               -> a -- ^Value for when key is pressed
               -> a -- ^Value for when key is released
               -> SF PlayerInput a
keyPressSwitch key downVal upVal = proc input -> do
    keyDownEvent <- keyDown key -< input
    keyUpEvent <- keyUp key -< input
    val <- hold upVal -< rMerge (keyDownEvent `tag` downVal) (keyUpEvent `tag` upVal)
    returnA -< val
    
-- |Handles movement via the directional keys 
keyboardMovement :: Double -- ^Value to move by along each axis
                 -> SF PlayerInput Vector2D
keyboardMovement x = proc input -> do
    upMovement <- keyPressSwitch SDL.SDLK_UP (-x) 0 -< input
    downMovement <- keyPressSwitch SDL.SDLK_DOWN x 0 -< input
    leftMovement <- keyPressSwitch SDL.SDLK_LEFT (-x) 0 -< input
    rightMovement <- keyPressSwitch SDL.SDLK_RIGHT x 0 -< input
    returnA -< vector2 (leftMovement + rightMovement) (upMovement + downMovement)     
    
------------------------
-- Mouse input events --
------------------------
-- |Signal function returning an event that fires when a specific mouse button is clicked
mouseClick :: SDL.MouseButton -- ^Mouse button to test
           -> SF PlayerInput (Yampa.Event ())
mouseClick button = edge <<< arr (any (isMouseClickEvent)) <<< arr (mouseEvents)
    where 
        mouseEvents = filter isMouseClickEvent
        isMouseClickEvent (SDL.MouseButtonDown _ _ button') = button == button'
        isMouseClickEvent _ = False

-- |Signal function returning the last known position of the mouse cursor
mousePosition :: SF PlayerInput Vector2D
mousePosition = hold zeroVector <<< arr mouseEvent
    where
        mouseEvent ev | null mouseEvents = Yampa.NoEvent
                      | otherwise = Yampa.Event $ mouseEventToPoint (last mouseEvents)
             where
                mouseEvents = filter isMouseMotionEvent ev
                isMouseMotionEvent (SDL.MouseMotion _ _ _ _) = True
                isMouseMotionEvent _ = False
                mouseEventToPoint (SDL.MouseMotion x y _ _) = vector2 (fromIntegral x) (fromIntegral y)
             
-- |SIgnal function returning the position the mouse was at when the specified button was last clicked             
mouseClickPosition :: SDL.MouseButton -- ^Mouse button to test
                   -> SF PlayerInput Vector2D
mouseClickPosition button = hold zeroVector <<< arr mouseEvent
    where
        mouseEvent ev | null mouseEvents = Yampa.NoEvent
                      | otherwise = Yampa.Event $ mouseEventToPoint (last mouseEvents)
             where
                mouseEvents = filter isMouseClickEvent ev
                isMouseClickEvent (SDL.MouseButtonDown _ _ button') = button == button'
                isMouseClickEvent _ = False
                mouseEventToPoint (SDL.MouseButtonDown x y _) = vector2 (fromIntegral x) (fromIntegral y)