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

module MtgPure.ModelCombinators
  ( AsAny,
    asAny,
    AsPermanent,
    asPermanent,
    AsCreaturePlayerPlaneswalker,
    asCreaturePlayerPlaneswalker,
    AsDamage (..),
    is,
    satisfies,
    spellCost,
    noCost,
    dealDamage,
    controllerOf,
    changeTo,
    sacrifice,
    destroy,
  )
where

import Data.Inst (Inst2, Inst3, Inst4)
import MtgPure.Model

type AsAny a =
  ToObject8
    a
    OTArtifact
    OTCreature
    OTEnchantment
    OTInstant
    OTLand
    OTPlaneswalker
    OTPlayer
    OTSorcery

asAny :: AsAny a => a -> OAny
asAny = toObject8

type AsPermanent a =
  ToObject5
    a
    OTArtifact
    OTCreature
    OTEnchantment
    OTLand
    OTPlaneswalker

asPermanent :: AsPermanent a => a -> OPermanent
asPermanent = toObject5

type AsCreaturePlayerPlaneswalker a =
  ToObject3
    a
    OTCreature
    OTPlaneswalker
    OTPlayer

asCreaturePlayerPlaneswalker :: AsCreaturePlayerPlaneswalker a => a -> OCreaturePlayerPlaneswalker
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

spellCost :: ToManaCost a => a -> Elect Cost x
spellCost = Cost . ManaCostCost . toManaCost

noCost :: Elect Cost a
noCost = Cost $ OrCosts []

dealDamage ::
  (AsAny source, AsCreaturePlayerPlaneswalker target, AsDamage damage) =>
  source ->
  target ->
  damage ->
  Effect 'OneShot
dealDamage source target = DealDamage (asAny source) (asCreaturePlayerPlaneswalker target) . asDamage

controllerOf :: AsAny o => o -> (OPlayer -> Elect e a) -> Elect e a
controllerOf = ControllerOf . asAny

sacrifice :: CoPermanent ot => OPlayer -> [Requirement ot] -> Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo :: (AsPermanent o, CoPermanent ot) => o -> Card ot -> Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

destroy :: CoPermanent ot => ObjectN ot -> Effect 'OneShot
destroy = Destroy coPermanent

class CoPermanent ot where
  coPermanent :: Permanent ot

instance CoPermanent OTArtifact where
  coPermanent = PermanentArtifact

instance CoPermanent OTCreature where
  coPermanent = PermanentCreature

instance CoPermanent OTEnchantment where
  coPermanent = PermanentEnchantment

instance CoPermanent OTLand where
  coPermanent = PermanentLand

instance CoPermanent OTPlaneswalker where
  coPermanent = PermanentPlaneswalker

instance CoPermanent OTPermanent where
  coPermanent = Permanent

instance Inst2 IsPermanentType a b => CoPermanent '(a, b) where
  coPermanent = Permanent2 :: Permanent '(a, b)

instance Inst3 IsPermanentType a b c => CoPermanent '(a, b, c) where
  coPermanent = Permanent3 :: Permanent '(a, b, c)

instance Inst4 IsPermanentType a b c d => CoPermanent '(a, b, c, d) where
  coPermanent = Permanent4 :: Permanent '(a, b, c, d)

class CoAny ot where
  coAny :: AnyObject ot

instance CoAny OTInstant where
  coAny = AnyInstant

instance CoAny OTSorcery where
  coAny = AnySorcery

instance CoAny OTPlayer where
  coAny = AnyPlayer

instance CoAny OTArtifact where
  coAny = AnyPermanent coPermanent

instance CoAny OTCreature where
  coAny = AnyPermanent coPermanent

instance CoAny OTEnchantment where
  coAny = AnyPermanent coPermanent

instance CoAny OTLand where
  coAny = AnyPermanent coPermanent

instance CoAny OTPlaneswalker where
  coAny = AnyPermanent coPermanent

instance CoAny OTPermanent where
  coAny = AnyPermanent coPermanent

instance Inst2 IsPermanentType a b => CoAny '(a, b) where
  coAny = AnyPermanent coPermanent

instance Inst3 IsPermanentType a b c => CoAny '(a, b, c) where
  coAny = AnyPermanent coPermanent

instance Inst4 IsPermanentType a b c d => CoAny '(a, b, c, d) where
  coAny = AnyPermanent coPermanent

is :: CoAny ot => ObjectN ot -> Requirement ot
is = Is coAny

satisfies :: CoAny ot => ObjectN ot -> [Requirement ot] -> Condition
satisfies = Satisfies coAny
