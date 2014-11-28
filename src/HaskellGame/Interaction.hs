module HaskellGame.Interaction where

import Prelude (
                Num(..), Eq(..), Show(..),
                Bool(..), Char(), Int(), Maybe(..),
                (||), (.), otherwise, not, fst, snd
               )

import qualified System.Console.ANSI as Console
import qualified Data.List as List
import Data.List ((++), (!!), elem, any, filter, null)

import HaskellGame.Datatypes
import HaskellGame.Graphics
import HaskellGame.Battle
import HaskellGame.Utils.Dictionary

{-
  Check if the player's new position would collide with something.
  Return (True, Just x) if there would be a collision with object x.
  Return (True, Nothing) if there would be a collision with a tile or a monster
  Return (False, Nothing) if there would be no collision.
-}

detectCollision :: Scene -> Point -> (Bool, Maybe Object)
detectCollision theScene (x, y) =
  let theLevel = currentLevel theScene
      theMap = map theLevel
      tile = ((tiles theMap) !! y) !! x
      nonDroppedItems = filter (not . isDroppedItem) (objects theLevel)
      objectPositions = List.map position nonDroppedItems
      monsterPositions = List.map position (monsters theLevel)
  in
    if any (== (x, y)) objectPositions then
      let theObject = List.head (filter ((== (x, y)) . position) (objects theLevel))
      in (True, Just theObject)
    else if (notWalkable tile) || (any (== (x, y)) monsterPositions) then
      (True, Nothing)
    else
      (False, Nothing)
  where
    notWalkable Grass = False
    notWalkable _     = True

{- Handle a key press from the player -}

handleInput :: Char -> Scene -> Scene
handleInput c theScene
  | c `elem` ['i', 'j', 'k', 'l'] = movePlayer c theScene
  | c == 'a'                      = doAttack theScene
  | c == 'p'                      = pickUpItem theScene
  | c == 'd'                      = dropItem theScene
  | otherwise                     = theScene
  where
    movePlayer :: Char -> Scene -> Scene
    movePlayer keyPressed oldScene =
      let (x, y) = position (player oldScene)
          newPosition = case keyPressed of
                          'i' -> (x, (y-1))
                          'j' -> ((x-1), y)
                          'k' -> (x, (y+1))
                          'l' -> ((x+1), y)
                          _   -> (x, y)
          newPlayer = (player oldScene) { pos = newPosition }
          isCollision = detectCollision oldScene newPosition
      in
        if (fst isCollision) then
          oldScene { collided = snd isCollision }
        else oldScene { player = newPlayer, collided = Nothing }

    doAttack :: Scene -> Scene
    doAttack oldScene =
      let thePlayer = player oldScene
          theMap = currentLevel oldScene
          allMonsters = monsters theMap
          nearbyMonsters = filter ((==1) . (distance thePlayer)) allMonsters
      in if not (null nearbyMonsters) then
           attackMonsters nearbyMonsters oldScene
         else oldScene { messages = (messages oldScene) ++ missedMessage }

    attackMonsters :: [Monster] -> Scene -> Scene
    --do nothing if there are no monsters
    attackMonsters [] x = x
    --if there are monsters, attack the first one
    attackMonsters (firstMonster:rest) battleScene =
      let oldPlayer = player battleScene
          (newPlayer, newMonster) = fight (oldPlayer, firstMonster)
          (damageP, damageM) = ((health oldPlayer - health newPlayer), (health firstMonster - health newMonster))
          battleMessages = hitMessage newMonster damageM newPlayer damageP
          -- now we need to update the scene to reflect what has happened
          oldLevel = currentLevel battleScene
          oldMonsters = monsters oldLevel
          newMonsters = (newMonster:(List.delete firstMonster oldMonsters))
          oldMessages = messages battleScene
          newMessages = oldMessages ++ battleMessages
          newLevel = oldLevel { monsters = newMonsters }
          newScene = battleScene { player = newPlayer, currentLevel = newLevel, messages = newMessages }
      in -- now we need to battle the next monster, and keep going until there are none left
        attackMonsters rest newScene

    pickUpItem :: Scene -> Scene
    pickUpItem oldScene = 
      
      let thePlayer = player oldScene
          theMap = currentLevel oldScene
          allObjects = objects theMap
	  newInventory = inventory thePlayer
          localObjects = filter ((==0) . (distance thePlayer)) allObjects
          localObject = List.find isDroppedItem localObjects
      in
        case (localObjects, localObject) of
          ([], _) -> oldScene {messages = (messages oldScene) ++ nothingToPickUpMessage}
          (_, Just (Dropped item _)) -> oldScene {messages = (messages oldScene) ++ pickUpMessage item}
					
	
    dropItem :: Scene -> Scene
    dropItem oldScene =
      let thePlayer = player oldScene
          newInventory = inventory thePlayer
          inventoryItem = List.head (toList newInventory)
          
          theMap = currentLevel oldScene
          allObjects = objects theMap
          localObjects = filter ((==0) . (distance thePlayer)) allObjects
          localObject = List.find isDroppedItem localObjects           
      in
          case (newInventory, inventoryItem,localObject, localObjects) of
            (empty, _, _, []) -> oldScene {messages = (messages oldScene) ++ nothingToDropMessage}
            (_, (_, x), Just (Dropped groundItem _), [a]) -> oldScene {messages = (messages oldScene) ++ cannotDropMessage x}
           -- (_, _, _, _) -> oldScene {messages = (messages oldScene) ++ nothingToPickUpMessage}  

    missedMessage :: [Message]
    missedMessage = [(Console.Red, "You flail wildly at empty space! Your attack connects with nothing.")]

    hitMessage :: Monster -> Int -> Player -> Int -> [Message]
    hitMessage monster monsterDamage player playerDamage =
      [(Console.Red, [symbol monster] ++ " hits " ++ [symbol player]  ++ " for " ++ show playerDamage  ++ " damage!"),
       (Console.Red, [symbol player]  ++ " hits " ++ [symbol monster] ++ " for " ++ show monsterDamage ++ " damage!")]

    pickUpMessage :: Item -> [Message]
    pickUpMessage thing = [(Console.Yellow, ("You pick up the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

    nothingToPickUpMessage :: [Message]
    nothingToPickUpMessage = [(Console.Yellow, "Nothing to pick up here!")]

    dropMessage :: Item -> [Message]
    dropMessage thing = [(Console.Yellow, ("You drop the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

    cannotDropMessage :: Item -> [Message]
    cannotDropMessage thing = [(Console.Yellow, ("No space here to drop the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

    nothingToDropMessage :: [Message]
    nothingToDropMessage = [(Console.Yellow, "You don't have anything to drop!")]
