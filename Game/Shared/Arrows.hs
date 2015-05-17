{-# LANGUAGE Arrows #-}

module Game.Shared.Arrows where

import Numeric.IEEE

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

-- |Signal function that returns an event which fires every update frame (1/5th of a second)
onFrameEvent :: SF () (Yampa.Event ())
onFrameEvent = repeatedly (1 / 5) ()

-- |Staggers an event. Gathers up a queue of x events, then each proceeding
-- event is queued and the event at the top of the list is returned.
staggerEvent :: Int -- ^Number of events to queue, must be positive
             -> SF (Yampa.Event a) (Yampa.Event a)
staggerEvent count | count < 0  = error "Negative count in staggerEvent."
                   | count == 0 = identity
                   | otherwise  = recGather []
    where
        recGather xs = switch (gather xs >>> second (delayEvent epsilon)) (recGather)
        gather [] = proc inEvent -> do
            returnA -< if isEvent inEvent 
                then (noEvent, Yampa.Event [inEvent])
                else (noEvent, noEvent)
        gather evts = proc inEvent -> do
            returnA -< if isEvent inEvent 
                then if length evts /= count
                    then (noEvent, Yampa.Event (tail evts ++ [inEvent]))
                    else (head evts, Yampa.Event (tail evts ++ [inEvent]))
                else (noEvent, noEvent)

-- |Performs integration on a scalar value
scalarIntegral :: RealFloat a => SF a a
scalarIntegral = proc value -> do
    intVal <- integral -< vector2 value value
    returnA -< vector2X intVal

-- |Tags an event with a Maybe value
tagMaybe :: Yampa.Event a -> Maybe b -> Yampa.Event b
tagMaybe Yampa.NoEvent _ = Yampa.NoEvent
tagMaybe (Yampa.Event _) Nothing = Yampa.NoEvent
tagMaybe (Yampa.Event _) (Just x) = Yampa.Event x

-- |Modifies an event according to a function. Equivilent to fmap
tagUsing :: Yampa.Event a -> (a -> b) -> Yampa.Event b
tagUsing Yampa.NoEvent _ = Yampa.NoEvent
tagUsing (Yampa.Event x) f = Yampa.Event (f x)

-- |Modifies a list of events according to a function
tagAllUsing :: [Yampa.Event a] -> (a -> b) -> Yampa.Event [b]
tagAllUsing xs f = catEvents (map (\x -> tagUsing x f) xs)

-- |Packs a list of values into an event where
-- an empty list returns a NoEvent
listToEvent :: [a] -> Yampa.Event [a]
listToEvent [] = noEvent
listToEvent xs = Yampa.Event xs

-- |Unpacks an event containing a list of values where
-- a NoEvent returns an empty list
eventToList :: Yampa.Event [a] -> [a]
eventToList Yampa.NoEvent = []
eventToList (Yampa.Event xs) = xs

-- Returns an event based on a condition. True will return an empty event,
-- where False will return a NoEvent
condEvent :: Bool -> Yampa.Event ()
condEvent True = Yampa.Event ()
condEvent False = noEvent

-- Creates an event based on a condition and then tags it with a value
taggedCondEvent :: Bool -> a -> Yampa.Event a
taggedCondEvent True val = Yampa.Event val
taggedCondEvent False _ = noEvent