module Main where

import Test.Hspec
import Test.QuickCheck

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Game.Shared.Types
import Game.Shared.Physics
import Game.Shared.Arrows
import Game.Shared.Object

import Data.Maybe

import Control.Exception (evaluate)

testObjects :: [GameObject]
testObjects = [basicGameObject { goId = 1, goTeam = Neutral, goType = Player },
	basicGameObject { goId = 2, goTeam = Red, goType = Minion },
	basicGameObject { goId = 3, goTeam = Red, goType = Turret }]

testRangedObjects :: [GameObject]
testRangedObjects = [basicGameObject { goId = 1, goPos = vector2 0 0, goSize = vector2 10 10 },
	basicGameObject { goId = 2, goPos = vector2 20 20, goSize = vector2 10 5 },
	basicGameObject { goId = 3, goPos = vector2 5 5, goSize = vector2 2 2 }]

main :: IO ()
main = hspec $ do
	describe "Game.Shared.Object.findObjById" $ do
		it "can find an object by its id" $ do
			findObjById 1 testObjects `shouldSatisfy` (\(Just val) -> (goId val) == 1)
			findObjById 2 testObjects `shouldSatisfy` (\(Just val) -> (goId val) == 2)
			findObjById 3 testObjects `shouldSatisfy` (\(Just val) -> (goId val) == 3)
		it "returns Nothing when no game object is found" $ do
			findObjById 0 testObjects `shouldSatisfy` isNothing
			findObjById 4 testObjects `shouldSatisfy` isNothing

	describe "Game.Shared.Object.findObjByTeam" $ do
		it "can find an object by its team" $ do
			findObjByTeam Neutral testObjects `shouldSatisfy` (\(Just val) -> (goTeam val) == Neutral)
			findObjByTeam Red testObjects `shouldSatisfy` (\(Just val) -> (goTeam val) == Red)
		it "returns Nothing when no game object is found" $ do
			findObjByTeam Blue testObjects `shouldSatisfy` isNothing

	describe "Game.Shared.Object.findObjByType" $ do
		it "can find an object by its type" $ do
			findObjByType Player testObjects `shouldSatisfy` (\(Just val) -> (goType val) == Player)
			findObjByType Turret testObjects `shouldSatisfy` (\(Just val) -> (goType val) == Turret)
		it "returns Nothing when no game object is found" $ do
			findObjByType TurretProjectile testObjects `shouldSatisfy` isNothing

	describe "Game.Shared.Object.findCollisions" $ do
		it "finds colliding pairs of objects" $ do
			findCollisions testRangedObjects `shouldSatisfy` (elem (testRangedObjects !! 0, testRangedObjects !! 2))
	
	describe "Game.Shared.Object.findWithinRangeOf" $ do
		it "finds objects within a specific range" $ do
			findWithinRangeOf (testRangedObjects !! 2) 10 testRangedObjects `shouldSatisfy` (elem (testRangedObjects !! 2))

	describe "Game.Shared.Object.findWithinRangeOfId" $ do
		it "finds objects within a specific range" $ do
			findWithinRangeofId 3 10 testRangedObjects `shouldSatisfy` (elem (testRangedObjects !! 2))

	describe "Game.Shared.Object.findWithinRangeOfPoint" $ do
		it "finds objects within a specific range" $ do
			findWithinRangeOfPoint (vector2 5 5) 10 testRangedObjects `shouldSatisfy` (elem (testRangedObjects !! 2))

	describe "Game.Shared.Object.filterObjByType" $ do
		it "returns no objects when no types specified" $ do
			filterObjByType [] testObjects `shouldBe` []
		it "can filter by single type" $ do
			filterObjByType [Player] testObjects `shouldSatisfy` (\xs -> (length xs) == 1)
		it "can filter by multiple types" $ do
			filterObjByType [Player, Minion] testObjects `shouldSatisfy` (\xs -> (length xs) == 2)

	describe "Game.Shared.Object.filterObjByTeam" $ do
		it "can filter by team" $ do
			filterObjByTeam Neutral testObjects `shouldSatisfy` (\xs -> (length xs) == 1)

	describe "Game.Shared.Object.sortByDistanceFrom" $ do
		it "sort objects correctly" $ do
			sortByDistanceFrom (testRangedObjects !! 0) testRangedObjects `shouldSatisfy` (\(x1:x2:x3:[]) -> x1 == (testRangedObjects !! 0)
			
	describe "Game.Shared.Object.sortByDistanceFromPoint" $ do
		it "sort objects correctly" $ do
			sortByDistanceFromPoint (zeroVector) testRangedObjects `shouldSatisfy` (\(x1:x2:x3:[]) -> x1 == (testRangedObjects !! 0)
			
	describe "Game.Shared.Types.tagMaybe" $ do
		it "returns NoEvent when no event is used with any value" $ do
			tagMaybe Yampa.NoEvent Nothing `shouldSatisfy` (isNoEvent :: Event () -> Bool)
			tagMaybe Yampa.NoEvent (Just 5) `shouldSatisfy` (isNoEvent :: Num a => Event a -> Bool)
		it "returns NoEvent when any event is used with Nothing" $ do
			tagMaybe (Yampa.Event 5) Nothing `shouldSatisfy` (isNoEvent :: Event () -> Bool)
			tagMaybe (Yampa.Event True) Nothing `shouldSatisfy` (isNoEvent :: Event () -> Bool)
		it "returns an event tagged with the specified value" $ do
			tagMaybe (Yampa.Event 5) (Just "Hi") `shouldBe` (Yampa.Event "Hi")
			tagMaybe (Yampa.Event True) (Just 5.5) `shouldBe` (Yampa.Event 5.5)
	
	describe "Game.Shared.Types.tagUsing" $ do
		it "returns NoEvent when no event is used as the input" $ do
			tagUsing Yampa.NoEvent (+5) `shouldSatisfy` (isNoEvent :: Num a => Event a -> Bool)
			tagUsing Yampa.NoEvent (++"Hi") `shouldSatisfy` (isNoEvent :: Event String -> Bool)
		it "transforms an input event by a given function" $ do
			tagUsing (Yampa.Event 5) (+5) `shouldBe` (Yampa.Event 10)
			tagUsing (Yampa.Event "Hello") (++", world!") `shouldBe` (Yampa.Event "Hello, world!")

	describe "Game.Shared.Types.vectorDistance" $ do
		it "returns distance between two vectors" $ do
			vectorDistance (vector2 0 0) (vector2 1 1) `shouldBe` (sqrt 2)
			vectorDistance (vector2 0 0) (vector2 0 1) `shouldBe` 1
			vectorDistance (vector2 0 0) (vector2 1 0) `shouldBe` 1

	describe "Game.Shared.Types.vectorComponents" $ do
		it "first value is equal to X" $ do
			fst (vectorComponents (vector2 1 2)) `shouldBe` 1
		it "second value is equal to Y" $ do
			snd (vectorComponents (vector2 1 2)) `shouldBe` 2
	
	describe "Game.Shared.Types.vectorRoundedComponents" $ do
		it "rounds values above to x.5 up" $ do
			vectorRoundedComponents (vector2 0.51 0.51) `shouldBe` (1, 1)
		it "rounds values below or equal x.5 down" $ do
			vectorRoundedComponents (vector2 0.5 0.5) `shouldBe` (0, 0)
	
	describe "Game.Shared.Types.lerpV" $ do
		it "returns start value when percent is 0.0" $ do
			lerpV (vector2 0 0) (vector2 1 1) 0 `shouldBe` (vector2 0 0)
		it "returns end value when percent is 1.0" $ do
			lerpV (vector2 0 0) (vector2 1 1) 1 `shouldBe` (vector2 1 1)	
		it "returns half way value when percent is 0.5" $ do
			lerpV (vector2 0 0) (vector2 1 1) 0.5 `shouldBe` (vector2 0.5 0.5)
	
	describe "Game.Shared.Physics.withinRange" $ do
		it "returns False when outside range" $ do
			withinRange 0 1 2 `shouldBe` False
			withinRange 3 1 2 `shouldBe` False
		it "returns True when within range" $
			withinRange 2 1 3 `shouldBe` True
		it "returns True when equal to upper or lowwer bound" $ do
			withinRange 1 1 3 `shouldBe` True
			withinRange 3 1 3 `shouldBe` True