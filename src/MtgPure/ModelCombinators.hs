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
    event,
    ifThen,
    ifElse,
    nonBasic,
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
    addToBattlefield,
    colored,
    colorless,
    tapped,
  )
where

import Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import MtgPure.Model

class AsWithObject ot where
  object :: [Requirement ot] -> (ObjectN ot -> x o) -> WithObject x o

instance Inst1 IsObjectType a => AsWithObject a where
  object = O1

instance Inst2 IsObjectType a b => AsWithObject '(a, b) where
  object = O2

instance Inst3 IsObjectType a b c => AsWithObject '(a, b, c) where
  object = O3

instance Inst4 IsObjectType a b c d => AsWithObject '(a, b, c, d) where
  object = O4

instance Inst5 IsObjectType a b c d e => AsWithObject '(a, b, c, d, e) where
  object = O5

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
spellCost = Cost . ManaCost . toManaCost

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
  coPermanent :: WPermanent ot

instance CoPermanent OTArtifact where
  coPermanent = WPermanentArtifact

instance CoPermanent OTCreature where
  coPermanent = WPermanentCreature

instance CoPermanent OTEnchantment where
  coPermanent = WPermanentEnchantment

instance CoPermanent OTLand where
  coPermanent = WPermanentLand

instance CoPermanent OTPlaneswalker where
  coPermanent = WPermanentPlaneswalker

instance CoPermanent OTPermanent where
  coPermanent = WPermanent

instance Inst2 IsPermanentType a b => CoPermanent '(a, b) where
  coPermanent = WPermanent2 :: WPermanent '(a, b)

instance Inst3 IsPermanentType a b c => CoPermanent '(a, b, c) where
  coPermanent = WPermanent3 :: WPermanent '(a, b, c)

instance Inst4 IsPermanentType a b c d => CoPermanent '(a, b, c, d) where
  coPermanent = WPermanent4 :: WPermanent '(a, b, c, d)

class CoAny ot where
  coAny :: WAny ot

instance CoAny OTInstant where
  coAny = WAnyInstant

instance CoAny OTSorcery where
  coAny = WAnySorcery

instance CoAny OTPlayer where
  coAny = WAnyPlayer

instance CoAny OTArtifact where
  coAny = WAnyPermanent coPermanent

instance CoAny OTCreature where
  coAny = WAnyPermanent coPermanent

instance CoAny OTEnchantment where
  coAny = WAnyPermanent coPermanent

instance CoAny OTLand where
  coAny = WAnyPermanent coPermanent

instance CoAny OTPlaneswalker where
  coAny = WAnyPermanent coPermanent

instance CoAny OTPermanent where
  coAny = WAnyPermanent coPermanent

instance Inst2 IsPermanentType a b => CoAny '(a, b) where
  coAny = WAnyPermanent coPermanent

instance Inst3 IsPermanentType a b c => CoAny '(a, b, c) where
  coAny = WAnyPermanent coPermanent

instance Inst4 IsPermanentType a b c d => CoAny '(a, b, c, d) where
  coAny = WAnyPermanent coPermanent

is :: CoAny ot => ObjectN ot -> Requirement ot
is = Is coAny

satisfies :: CoAny ot => ObjectN ot -> [Requirement ot] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent ot => OPlayer -> [Requirement ot] -> Cost
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent ot => Requirement ot
tapped = Tapped coPermanent

addToBattlefield :: CoPermanent a => OPlayer -> Token a -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: ColorsLike c => c -> Requirement ot
ofColors = OfColors . toColors

class AsCost c where
  asCost :: c -> Cost

instance AsCost Cost where
  asCost = id

instance AsCost ManaCost where
  asCost = ManaCost

playerPays :: AsCost c => c -> Requirement OTPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect ot

instance ElectEffect (Effect e) (Elect e) where
  effect = Effect . pure

instance ElectEffect [Effect e] (Elect e) where
  effect = Effect

event :: EventListener a -> Elect EventListener a
event = Event

class Branchable e where
  branchEmpty :: Elect e a

instance Branchable Cost where
  branchEmpty = Cost $ AndCosts []

instance Branchable EventListener where
  branchEmpty = Event $ Events []

instance Branchable (e :: EffectType) where
  branchEmpty = Effect []

ifThen :: Branchable e => Condition -> Elect e a -> Elect e a
ifThen cond elect = If cond elect branchEmpty

ifElse :: Branchable e => Condition -> Elect e a -> Elect e a
ifElse cond = If cond branchEmpty

nonBasic :: Requirement OTLand
nonBasic = RAnd $ map (Not . HasBasicLandType) [minBound ..]

colored :: Requirement ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: Requirement ot
colorless = Not colored
