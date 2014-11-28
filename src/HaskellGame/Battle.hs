module HaskellGame.Battle where

import Prelude (
                Num(..), Eq(..), Ord(..),
                Int(), Maybe(..),
                (.), fst, otherwise, floor, log, sum, snd, div, fromIntegral
               )
import qualified Data.List as List
import Data.List (find)

import HaskellGame.Datatypes

{-
   Let's define what we need to be able to do with a Combatant.
   We need to be able to get its health, compute attack and defense,
   and deal damage to it. We'll define this interface with a type class.
-}

class Combatant a where
  health  :: a -> Int
  attack  :: a -> Int
  defense :: a -> Int
  level   :: a -> Int
  damage  :: a -> Int -> a

{-
   Now we need a battle system!
   Let's build the simplest thing that could possibly work.
   At each step of the battle, the two combatants damage each other.
-}

fight :: (Combatant a, Combatant b) => (a, b) -> (a, b)
fight (fighter1, fighter2) =
  let damageTo1 = ((attack fighter2) - (defense fighter1))
      damageTo2 = ((attack fighter1) - (defense fighter2))
  in (damage fighter1 damageTo1, damage fighter2 damageTo2)

{-
   Now we need to give instances to say how Players and Monsters implement
   the operations of the Combatant type class, so that our fight function
   can operate on them.
-}

instance Combatant Player where
  health (Player h _ _ _ _ _ _) = h

  attack (Player _ _ stats skills _ _ _) =
    let (Just (_, str)) = find ((=="Strength") . fst) stats
        (Just (_, fcs)) = find ((=="Fisticuffs") . fst) skills
    in str * fcs

  defense (Player _ _ stats _ _ _ _) =
    let (Just (_, def)) = find ((=="Toughness") . fst) stats
    in def

  level (Player _ _ stats _ _ _ _) =
    floor (log (fromIntegral (sum (List.map snd stats))))

  damage p dmg
    | dmg < 0   = p
    | otherwise = p { hitpoints = ((hitpoints p) - dmg) }

instance Combatant Monster where
  health m =
    case m of
      (Dragon h _ _) -> h
      (Zombie h _ _) -> h

  attack m =
    case m of
      (Dragon _ a _) -> a
      (Zombie _ a _) -> a

  defense m =
    case m of
      (Dragon _ d _) -> d
      (Zombie _ d _) -> d

  level m = 1 + ((health m + attack m + defense m) `div` 3)

  damage m dmg
    | dmg < 0 = m
    | otherwise =
        case m of
          (Dragon h x y) -> (Dragon (h-dmg) x y)
          (Zombie h x y) -> (Zombie (h-dmg) x y)
