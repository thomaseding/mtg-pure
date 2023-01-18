{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CreatureType (
  CreatureType (..),
) where

import safe Data.Typeable (Typeable)

data CreatureType
  = Bear
  | Bird
  | Devil
  | Eldrazi
  | Elf
  | Goblin
  | Horror
  | Human
  | Mercenary
  | Merfolk
  | Nomad
  | Rebel
  | Satyr
  | Shaman
  | Soldier
  | Zombie
  deriving (Eq, Ord, Show, Typeable)
