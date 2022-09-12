{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CreatureType
  ( CreatureType (..),
  )
where

data CreatureType
  = Bird
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
  | Soldier
  | Zombie
  deriving (Eq, Ord, Show)
