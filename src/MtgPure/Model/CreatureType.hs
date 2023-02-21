{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CreatureType (
  CreatureType (..),
) where

import safe Data.Typeable (Typeable)

data CreatureType
  = Bear
  | Beast
  | Bird
  | Cat
  | Devil
  | Dragon
  | Druid
  | Eldrazi
  | Elemental
  | Elf
  | Faerie
  | Goblin
  | Horror
  | Human
  | Mercenary
  | Merfolk
  | Nomad
  | Rebel
  | Satyr
  | Shaman
  | Slug
  | Soldier
  | Thopter
  | Wall
  | Zombie
  deriving (Eq, Ord, Show, Typeable)
