module HaskellGame.Graphics where

import Prelude ( Show(..) )
import qualified Data.List as List
import Data.List ((++))
import Data.Char (toUpper)

import HaskellGame.Datatypes
import HaskellGame.Utils

{- How to display active game elements -}

instance Graphic Player where
  symbol (Player _ _ _ _ _ _ _) = '☃'

instance Graphic Object where
  symbol (Chest _) = '?'
  symbol (Ladder _ _) = '_'
  symbol (Dropped i _) = symbol i

instance Graphic Item where
  symbol (Item _ _ i _) = i

instance Graphic Monster where
  symbol (Dragon _ _ _) = '🐉'
  symbol (Zombie _ _ _) = '💀'

{- Displaying stats and status -}

instance Show Stat where
  show (name, value) =
    (List.map toUpper (takesome 3 name)) ++
     ": " ++
     show value ++
     " "
