{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified System.Console.ANSI as Console
import qualified Data.List as List

import HaskellGame.Datatypes
import HaskellGame.Utils
import HaskellGame.Interaction
import HaskellGame.Utils.Dictionary

deriving instance Show Scene
deriving instance Show Map
deriving instance Show Level
deriving instance Show Monster
deriving instance Show Player
deriving instance Show Object
deriving instance Show Destination
deriving instance Show Item

deriving instance Eq Scene
deriving instance Eq Player

deriving instance Ord Monster
{- HUnit Tests -}

test_no_collide_walls =
  let theMap = "###" ++
               "#.#" ++
               "###"
      thePlayer = Player 0 0 [] [] (1, 1) (fromList []) (fromList [])
      theScene = (Scene (Level (createMap "Level 1" 3 3 theMap) [] []) [] thePlayer [] Nothing)
  in
    theScene @=? (handleInput 'j' theScene)


test_no_collide_objects =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      thePlayer = Player 0 0 [] [] (1, 1) (fromList []) (fromList [])
      theScene = (Scene (Level (createMap "Level 1" 3 4 theMap) [Chest (1,2)] []) [] thePlayer [] (Just (Chest (1,2))) )
  in
    theScene @=? (handleInput 'k' theScene)

test_movement =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      thePlayer = Player 0 0 [] [] (1, 1) (fromList []) (fromList [])
      theScene = (Scene (Level (createMap "Level 1" 3 4 theMap) [] []) [] thePlayer [] Nothing)
      expectedScene = (Scene (Level (createMap "Level 1" 3 4 theMap) [] []) [] (thePlayer { pos = (1, 2) }) [] Nothing)
  in
    expectedScene @=? (handleInput 'k' theScene)

{- Test that attacking works as expected -}

-- Check that attacking nothing works
test_attack_nothing =
  let theMap = "###" ++
               "#.#" ++
               "###"
      thePlayer = Player 0 0 [] [] (1, 1) (fromList []) (fromList [])
      theScene = (Scene (Level (createMap "Level 1" 3 3 theMap) [] []) [] thePlayer [] Nothing)
      expectedScene = theScene { messages = [(Console.Red, "You flail wildly at empty space! Your attack connects with nothing.")] }
  in
    expectedScene @=? (handleInput 'a' theScene)

-- Check that the correct amount of damage is done, and the correct
-- messages are reported when a player attacks a monster
test_attack_one =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      thePlayer = Player 10 0 [ ("Strength", 5), ("Toughness", 1) ] [ ("Fisticuffs", 1) ] (1, 1) (fromList []) (fromList [])
      theMonster = Dragon 20 2 (1, 2)
      theScene = (Scene (Level (createMap "Level 1" 3 4 theMap) [] [theMonster]) [] thePlayer [] Nothing)
      expectedPlayer = thePlayer { hitpoints = (hitpoints thePlayer) - 1 }
      expectedMonster = Dragon 17 2 (1, 2)
      expectedMessages = [(Console.Red, "🐉 hits ☃ for 1 damage!"),
                          (Console.Red, "☃ hits 🐉 for 3 damage!")]
      newScene = handleInput 'a' theScene
  in do
    expectedPlayer @=? (player newScene)
    expectedMonster @=? (head $ monsters $ currentLevel newScene)
    expectedMessages @=? (messages newScene)

-- Check that attacking multiple monsters works as expected
test_attack_several =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [ ("Strength", 5), ("Toughness", 1) ] [ ("Fisticuffs", 1) ] (2, 1) (fromList []) (fromList [])
      theMonsters = [Dragon 20 2 (1, 1), Dragon 20 2 (3, 1)]
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [] theMonsters) [] thePlayer [] Nothing)
      expectedPlayer = thePlayer { hitpoints = (hitpoints thePlayer) - 2 }
      expectedMonsters = [Dragon 17 2 (1, 1), Dragon 17 2 (3, 1)]
      expectedMessages = [(Console.Red, "🐉 hits ☃ for 1 damage!"),
                          (Console.Red, "☃ hits 🐉 for 3 damage!"),
                          (Console.Red, "🐉 hits ☃ for 1 damage!"),
                          (Console.Red, "☃ hits 🐉 for 3 damage!")]
      newScene = handleInput 'a' theScene
  in do
    expectedPlayer @=? (player newScene)
    (List.sort expectedMonsters) @=? (List.sort $ monsters $ currentLevel newScene) --they could be reordered so sort both first
    expectedMessages @=? (messages newScene)

-- Check that attacking monsters only attacks those in range
test_attack_range =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [ ("Strength", 5), ("Toughness", 1) ] [ ("Fisticuffs", 1) ] (2, 1) (fromList []) (fromList [])
      theMonsters = [Dragon 20 2 (1, 1), Dragon 20 2 (1, 3)]
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [] theMonsters) [] thePlayer [] Nothing)
      expectedPlayer = thePlayer { hitpoints = (hitpoints thePlayer) - 1 }
      expectedMonsters = [Dragon 17 2 (1, 1), Dragon 20 2 (1, 3)]
      expectedMessages = [(Console.Red, "🐉 hits ☃ for 1 damage!"),
                          (Console.Red, "☃ hits 🐉 for 3 damage!")]
      newScene = handleInput 'a' theScene
  in do
    expectedPlayer @=? (player newScene)
    (List.sort expectedMonsters) @=? (List.sort $ monsters $ currentLevel newScene) --they could be reordered so sort both first
    expectedMessages @=? (messages newScene)

test_pick_up_something =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [] [] (2, 1) (fromList []) (fromList [])
      theItem = Item "Item" "Looks itemish" '@' (fromList [("Itemness", 9001)])
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [Dropped theItem (2, 1)] []) [] thePlayer [] Nothing)

      expectedPlayer = thePlayer { inventory = fromList [(itemName theItem, theItem)] }
      expectedObjects = []
      expectedMessages = [(Console.Yellow, ("You pick up the Item, @"))]

      newScene = handleInput 'p' theScene --try to pick the thing up
  in do
    expectedPlayer @=? (player newScene)
    expectedObjects @=? (objects (currentLevel newScene))
    expectedMessages @=? (messages newScene)

test_pick_up_nothing =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [] [] (2, 1) (fromList []) (fromList [])
      theItem = Item "Item" "Looks itemish" '@' (fromList [("Itemness", 9001)])
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [Dropped theItem (1, 1)] []) [] thePlayer [] Nothing)
      expectedScene = theScene { messages = [(Console.Yellow, "Nothing to pick up here!")]}
      newScene = handleInput 'p' theScene --try to pick the thing up
  in do
    expectedScene @=? newScene

test_drop =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [] [] (2, 1) (fromList [(itemName theItem, theItem)]) (fromList [])
      theItem = Item "Item" "Looks itemish" '@' (fromList [("Itemness", 9001)])
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [] []) [] thePlayer [] Nothing)

      expectedPlayer = thePlayer { inventory = fromList [] }
      expectedObjects = [Dropped theItem (2, 1)]
      expectedMessages = [(Console.Yellow, ("You drop the Item, @"))]

      newScene = handleInput 'd' theScene --try to drop something
  in do
    expectedPlayer @=? (player newScene)
    expectedObjects @=? (objects (currentLevel newScene))
    expectedMessages @=? (messages newScene)

test_cannot_drop =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [] [] (2, 1) (fromList [(itemName theItem, theItem)]) (fromList [])
      theItem = Item "Item" "Looks itemish" '%' (fromList [("Itemness", 9001)])
      otherItem = theItem { itemName = "OtherItem", icon = '@' }
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [Dropped otherItem (2, 1)] []) [] thePlayer [] Nothing)

      expectedPlayer = thePlayer
      expectedObjects = [Dropped otherItem (2, 1)]
      expectedMessages = [(Console.Yellow, ("No space here to drop the Item, %"))]

      newScene = handleInput 'd' theScene --try to drop something
  in do
    expectedPlayer @=? (player newScene)
    expectedObjects @=? (objects (currentLevel newScene))
    expectedMessages @=? (messages newScene)

test_nothing_drop =
  let theMap = "#####" ++
               "#...#" ++
               "#...#" ++
               "#####"
      thePlayer = Player 10 0 [] [] (2, 1) (fromList []) (fromList [])
      theScene = (Scene (Level (createMap "Level 1" 5 4 theMap) [] []) [] thePlayer [] Nothing)

      expectedPlayer = thePlayer
      expectedObjects = []
      expectedMessages = [(Console.Yellow, "You don't have anything to drop!")]

      newScene = handleInput 'd' theScene --try to drop something
  in do
    expectedPlayer @=? (player newScene)
    expectedObjects @=? (objects (currentLevel newScene))
    expectedMessages @=? (messages newScene)

{- QuickCheck Tests -}

prop_takesome_take :: Int -> Bool
prop_takesome_take n =
  let xs = [0..9]
  in takesome n xs == take n xs

prop_dropsome_drop :: Int -> Bool
prop_dropsome_drop n =
  let xs = [0..9]
  in dropsome n xs == drop n xs

main = defaultMain tests

tests :: [TF.Test]
tests = [
          testGroup "Test Collisions" [
            testCase "Wall Collision" test_no_collide_walls,
            testCase "Object Collision" test_no_collide_objects,
            testCase "Player Movement" test_movement
          ],

          testGroup "Test Monster Battling" [
            testCase "Attacking nothing works" test_attack_nothing,
            testCase "Attacking one monster works" test_attack_one,
            testCase "Attacking several monsters works" test_attack_several,
            testCase "Attacking doesn't hit monsters out of range" test_attack_range
          ],

          testGroup "Test picking up and dropping items [50 marks]" [
            testCase "Player can pick up something from the ground if standing on it [10 marks]" test_pick_up_something,
            testCase "Player can't pick up something unless standing on it [10 marks]" test_pick_up_nothing,
            testCase "Player can drop something on the ground if there is space [10 marks]" test_drop,
            testCase "Player cannot drop something if there is something there already [10 marks]" test_cannot_drop,
            testCase "Player cannot drop something if they have nothing [10 marks]" test_nothing_drop
          ],

          testGroup "takesome and dropsome" [
            testProperty "takesome works like Prelude.take" prop_takesome_take,
            testProperty "dropsome works like Prelude.drop" prop_dropsome_drop
          ]
        ]
