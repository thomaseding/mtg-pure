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
  ( AsAny,
    asAny,
    AsActivatedOrTriggeredAbility,
    asActivatedOrTriggeredAbility,
    AsPermanent,
    asPermanent,
    AsSpell,
    asSpell,
    AsCreaturePlayerPlaneswalker,
    asCreaturePlayerPlaneswalker,
    AsDamage (..),
    AsCost (..),
    ElectEffect (..),
    CoAny (..),
    CoPermanent (..),
    AsWithObject (..),
    object,
    tapCost,
    ofColors,
    playerPays,
    is,
    satisfies,
    spellCost,
    noCost,
    dealDamage,
    controllerOf,
    changeTo,
    sacrifice,
    destroy,
    counterAbility,
    counterSpell,
    sacrificeCost,
    tapped,
  )
where

import Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import MtgPure.Model

class AsWithObject ot where
  withObject :: [Requirement ot] -> (ObjectN ot -> x o) -> WithObject x o

instance Inst1 IsObjectType a => AsWithObject a where
  withObject = O1

instance Inst2 IsObjectType a b => AsWithObject '(a, b) where
  withObject = O2

instance Inst3 IsObjectType a b c => AsWithObject '(a, b, c) where
  withObject = O3

instance Inst4 IsObjectType a b c d => AsWithObject '(a, b, c, d) where
  withObject = O4

instance Inst5 IsObjectType a b c d e => AsWithObject '(a, b, c, d, e) where
  withObject = O5

object :: AsWithObject ot => [Requirement ot] -> (ObjectN ot -> x o) -> WithObject x o
object = withObject

type AsActivatedOrTriggeredAbility a =
  ToObject2
    a
    OTActivatedAbility
    OTTriggeredAbility

asActivatedOrTriggeredAbility :: AsActivatedOrTriggeredAbility a => a -> OActivatedOrTriggeredAbility
asActivatedOrTriggeredAbility = toObject2

type AsAny a =
  ToObject12
    a
    OTActivatedAbility
    OTArtifact
    OTCreature
    OTEmblem
    OTEnchantment
    OTInstant
    OTLand
    OTPlaneswalker
    OTPlayer
    OTSorcery
    OTStaticAbility
    OTTriggeredAbility

asAny :: AsAny a => a -> OAny
asAny = toObject12

type AsDamageSource a =
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

asDamageSource :: AsDamageSource a => a -> ODamageSource
asDamageSource = toObject8

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

type AsSpell a =
  ToObject6
    a
    OTArtifact
    OTCreature
    OTEnchantment
    OTInstant
    OTPlaneswalker
    OTSorcery

asSpell :: AsSpell a => a -> OSpell
asSpell = toObject6

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
  (AsDamageSource source, AsCreaturePlayerPlaneswalker target, AsDamage damage) =>
  source ->
  target ->
  damage ->
  Effect 'OneShot
dealDamage source target = DealDamage (asDamageSource source) (asCreaturePlayerPlaneswalker target) . asDamage

controllerOf :: AsAny o => o -> (OPlayer -> Elect e a) -> Elect e a
controllerOf = ControllerOf . asAny

sacrifice :: CoPermanent ot => OPlayer -> [Requirement ot] -> Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo :: (AsPermanent o, CoPermanent ot) => o -> Card ot -> Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

tapCost :: AsPermanent o => o -> Cost
tapCost = TapCost . asPermanent

destroy :: AsPermanent o => o -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility :: AsActivatedOrTriggeredAbility o => o -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell o => o -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

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

sacrificeCost :: CoPermanent ot => OPlayer -> [Requirement ot] -> Cost
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent ot => Requirement ot
tapped = Tapped coPermanent

ofColors :: ColorsLike c => c -> Requirement ot
ofColors = OfColors . toColors

class AsCost c where
  asCost :: c -> Cost

instance AsCost Cost where
  asCost = id

instance AsCost ManaCost where
  asCost = ManaCostCost

playerPays :: AsCost c => c -> Requirement OTPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect ot

instance ElectEffect (Effect e) (Elect e) where
  effect = Effect . pure

instance ElectEffect [Effect e] (Elect e) where
  effect = Effect
