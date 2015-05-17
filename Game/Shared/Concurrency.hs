module Game.Shared.Concurrency where

import Control.Concurrent.MVar

import Data.Serialize
import Data.List

-----------------
-- Concurrency --
-----------------
-- |Initialises a list-type MVar
initMVar :: IO (MVar [a])
initMVar = newMVar []

-- |Pushes a value to an MVar
pushMVar :: MVar [a] -- ^MVar to push value to
         -> a -- ^The value to push
         -> IO ()
pushMVar var x = modifyMVar_ var (\vals -> return (x : vals))

-- |Pushes a list of values to an MVar
pushAllMVar :: MVar [a] -- ^MVar to push values to
            -> [a] -- ^List of values to push
            -> IO ()
pushAllMVar var xs = modifyMVar_ var (\vals -> return (xs ++ vals))

-- |Pops every value from an MVar
popAllMVar :: MVar [a] -- ^The MVar to pop values from
           -> IO [a] -- ^The values that were in the MVar
popAllMVar var = swapMVar var []

-- |Removes a specific value from an MVar
removeFromMVar :: Eq a => MVar [a] -- ^MVar to remove value from
               -> a -- ^The value to remove
               -> IO ()
removeFromMVar var val = modifyMVar_ var (\vals -> return (delete val vals))