{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.ModelCombinators
  ( AsPermanent (..),
    AsAny (..),
    AsCreaturePlayerPlaneswalker (..),
    AsDamage (..),
    spellCost,
    noCost,
    dealDamage,
    controllerOf,
  )
where

import MtgPure.Model

class AsAny a where
  asAny :: a -> OAny

instance AsAny OAny where
  asAny = id

instance AsAny OArtifact where
  asAny = O8a

instance AsAny OCreature where
  asAny = O8b

instance AsAny OEnchantment where
  asAny = O8c

instance AsAny OInstant where
  asAny = O8d

instance AsAny OLand where
  asAny = O8e

instance AsAny OPlaneswalker where
  asAny = O8f

instance AsAny OPlayer where
  asAny = O8g

instance AsAny OSorcery where
  asAny = O8h

class AsPermanent a where
  asPermanent :: a -> OPermanent

instance AsPermanent OArtifact where
  asPermanent = O5a

instance AsPermanent OCreature where
  asPermanent = O5b

instance AsPermanent OEnchantment where
  asPermanent = O5c

instance AsPermanent OLand where
  asPermanent = O5d

instance AsPermanent OPlaneswalker where
  asPermanent = O5e

class AsCreaturePlayerPlaneswalker a where
  asCreaturePlayerPlaneswalker :: a -> OCreaturePlayerPlaneswalker

instance AsCreaturePlayerPlaneswalker OCreaturePlayerPlaneswalker where
  asCreaturePlayerPlaneswalker = id

instance AsCreaturePlayerPlaneswalker OCreature where
  asCreaturePlayerPlaneswalker = O3a

instance AsCreaturePlayerPlaneswalker OPlaneswalker where
  asCreaturePlayerPlaneswalker = O3b

instance AsCreaturePlayerPlaneswalker OPlayer where
  asCreaturePlayerPlaneswalker = O3c

instance AsCreaturePlayerPlaneswalker OCreaturePlayer where
  asCreaturePlayerPlaneswalker = toObject3

instance AsCreaturePlayerPlaneswalker OCreaturePlaneswalker where
  asCreaturePlayerPlaneswalker = toObject3

instance AsCreaturePlayerPlaneswalker OPlayerPlaneswalker where
  asCreaturePlayerPlaneswalker = toObject3

class AsDamage a where
  asDamage :: a -> Damage

instance AsDamage Integer where
  asDamage n = asDamage (fromInteger n :: Int)

instance AsDamage Int where
  asDamage = Damage

instance AsDamage Damage where
  asDamage = id

instance AsDamage Variable where
  asDamage = VariableDamage

spellCost :: ToManaCost a => a -> (OPlayer -> Cost)
spellCost mana _you = ManaCostCost $ toManaCost mana

noCost :: OPlayer -> Cost
noCost _you = OrCosts []

dealDamage ::
  (AsAny source, AsCreaturePlayerPlaneswalker target, AsDamage damage) =>
  source ->
  target ->
  damage ->
  Effect 'OneShot
dealDamage source target = DealDamage (asAny source) (asCreaturePlayerPlaneswalker target) . asDamage

controllerOf :: AsAny o => o -> (OPlayer -> Elect e a) -> Elect e a
controllerOf = ControllerOf . asAny
