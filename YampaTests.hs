module Main where

import Control.Applicative

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Game.Client.Components.BasicComponents
import Game.Shared.Object
import Game.Shared.Networking

-- |Entrypoint for the tests
main :: IO ()
main = do
    putStrLn "Performing tests..."
    putStrLn ("SF `objectStats` passed tests: " ++ show test_objectStats)

-- | Runs a test on a Yampa signal function
runYampaTest :: SF a b -- ^Signal function to runall all 
             -> a -- ^Initial input value
             -> [(DTime, a)] -- ^Inputs (both the time, and individual value)
             -> [(b -> Bool)] -- ^Output validation functions
             -> Bool -- ^True if the actual outputs matched the expected outputs
runYampaTest sf init inputs outputs = allResultsMatch
    where
        inputValues = map secMaybe inputs
        results = embed sf (init, inputValues)  
        resultMatches = outputs <*> results
        allResultsMatch = all (==True) resultMatches
        secMaybe (a, b) = (a, Just b)

-- |The initial input value
test_objectStats_initialInput :: ObjectStatsInput
test_objectStats_initialInput = ObjectStatsInput { osiNetwork = [] }
        
-- |The list of inputs at certain times
test_objectStats_inputs :: [(DTime, ObjectStatsInput)]
test_objectStats_inputs = [(0, ObjectStatsInput { osiNetwork = [Message (UpdateStats 1 Health 50)] })]

-- |The list of predicate functions to test each of the previously defined inputs
test_objectStats_outputs :: [(ObjectStatsOutput -> Bool)]
test_objectStats_outputs = [(\out -> isEvent (osoHealthChanged out))]

-- |Setups the environment to test the object stats function
test_objectStats :: Bool
test_objectStats = runYampaTest (objectStats testObject) 
                                test_objectStats_initialInput 
                                test_objectStats_inputs 
                                test_objectStats_outputs
    where
        testObject = basicGameObject {
                goId = 1,
                goStats = Just testStats
            }
        testStats = basicStats {
                stHealth = 0,
                stMaxHealth = 100
            }
           