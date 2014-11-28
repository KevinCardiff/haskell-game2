module HaskellGame.Datatypes where

import Prelude (
                Read(..), Show(..), Eq(..), Num(..),  -- we need these type classes out of the Prelude
                Int(), Integer(), String(), Bool(..), Char(), Maybe(..),   -- we need these data types
                fst, (.), ($)
               )

import qualified System.Console.ANSI as Console
import System.IO (Handle())

import HaskellGame.Utils.Dictionary

{- Data types -}

data Tile = Grass
          | Wall
          | Empty
          deriving Eq

data Map = Map {
                 name :: String,
                 width :: Int,
                 height :: Int,
                 tiles :: [[Tile]]
               }
               deriving Eq

data Level = Level {
                     map :: Map,
                     objects :: [Object],
                     monsters :: [Monster]
                   }
                   deriving Eq

data Destination = Destination String Point
                 deriving Eq

type Point = (Int, Int)

data Object = Chest Point
            | Ladder Point Destination
            | Dropped Item Point
            deriving Eq

data Item = Item {
                   itemName :: String,
                   description :: String,
                   icon :: Char,
                   attributes :: SimpleDict String Int
                 }
          deriving Eq

data Monster = Dragon Int Int Point
             | Zombie Int Int Point
             deriving Eq

type Skill = (String, Int)

type Stat = (String, Int)

data Player = Player {
                       hitpoints :: Int,
                       experience :: Int,
                       stats :: [Stat],
                       skills :: [Skill],
                       pos :: Point,
                       inventory :: SimpleDict String Item,
                       slots :: SimpleDict String (Maybe Item)
                     }

data Scene = Scene {
                     currentLevel :: Level,
                     otherLevels :: [Level],
                     player :: Player,
                     messages :: [Message],
                     collided :: Maybe Object
                   }

-- We want nice colourful messages, so for each Message,
-- we have a colour as well as the string to print out.
type Message = (Console.Color, String)

data EngineState = EngineState {
                                 screen :: Handle,
                                 keyboard :: Handle,
                                 frameRate :: Integer,
                                 frameNumber :: Integer,
                                 keyPressed :: Char,
                                 isGameOver :: Bool,
                                 messageLimit :: Int,
                                 scene :: Scene
                               }

{- Type classes -}

class Located a where
  position :: a -> Point
  distance :: Located b => a -> b -> Int

  distance a b =
    let (x1, y1) = position a
        (x2, y2) = position b
    in abs(x1 - x2) + abs(y1 - y2)

class Graphic a where
  symbol :: a -> Char

{- Type class instances -}

instance Located Object where
  position (Chest pt) = pt
  position (Ladder pt _) = pt
  position (Dropped _ pt) = pt

instance Located Monster where
  position (Dragon _ _ pt) = pt
  position (Zombie _ _ pt) = pt

instance Located Player where
  position p = pos p

{- How to read and display the game map -}

instance Read Tile where
  readsPrec _ "." = [(Grass, "")]
  readsPrec _ "#" = [(Wall, "")]
  readsPrec _ _   = [(Empty, "")]

instance Show Tile where
  show Grass        = "."
  show Wall         = "#"
  show Empty        = " "

{- We'll need these functions in other files -}

isChest, isLadder, isDroppedItem :: Object -> Bool

isChest (Chest _) = True
isChest _ = False

isLadder (Ladder _ _) = True
isLadder _ = False

isDroppedItem (Dropped _ _) = True
isDroppedItem _ = False
