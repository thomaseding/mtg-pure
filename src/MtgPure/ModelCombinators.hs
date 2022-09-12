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

import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import safe MtgPure.Model.Color (Color)
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType
  ( OT,
    ObjectType (..),
    ObjectType1,
    ObjectType2,
    ObjectType3,
    ObjectType4,
    ObjectType5,
  )
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPermanent,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
  )
import safe MtgPure.Model.ObjectType.Permanent (IsPermanentType, WPermanent (..))
import safe MtgPure.Model.Recursive
  ( Card,
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    EventListener (..),
    Requirement (..),
    Token,
    TypeableOT,
    TypeableOT2,
    WithObject (..),
  )
import safe MtgPure.Model.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.ToObjectN.Classes
  ( ToObject12 (..),
    ToObject2 (..),
    ToObject3 (..),
    ToObject5 (..),
    ToObject6 (..),
    ToObject8 (..),
  )
import safe MtgPure.Model.Variable (Variable)

class AsWithObject a where
  object :: TypeableOT2 k ot x => [Requirement a] -> (ObjectN a -> x ot) -> WithObject x ot

instance Inst1 IsObjectType a => AsWithObject '(OT, a) where
  object = O1

instance Inst2 IsObjectType a b => AsWithObject '(OT, a, b) where
  object = O2

instance Inst3 IsObjectType a b c => AsWithObject '(OT, a, b, c) where
  object = O3

instance Inst4 IsObjectType a b c d => AsWithObject '(OT, a, b, c, d) where
  object = O4

instance Inst5 IsObjectType a b c d e => AsWithObject '(OT, a, b, c, d, e) where
  object = O5

type AsActivatedOrTriggeredAbility a =
  ToObject2
    a
    'OTActivatedAbility
    'OTTriggeredAbility

asActivatedOrTriggeredAbility :: AsActivatedOrTriggeredAbility a => a -> OActivatedOrTriggeredAbility
asActivatedOrTriggeredAbility = toObject2

type AsAny a =
  ToObject12
    a
    'OTActivatedAbility
    'OTArtifact
    'OTCreature
    'OTEmblem
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery
    'OTStaticAbility
    'OTTriggeredAbility

asAny :: AsAny a => a -> OAny
asAny = toObject12

type AsDamageSource a =
  ToObject8
    a
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery

asDamageSource :: AsDamageSource a => a -> ODamageSource
asDamageSource = toObject8

type AsPermanent a =
  ToObject5
    a
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTLand
    'OTPlaneswalker

asPermanent :: AsPermanent a => a -> OPermanent
asPermanent = toObject5

type AsSpell a =
  ToObject6
    a
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTPlaneswalker
    'OTSorcery

asSpell :: AsSpell a => a -> OSpell
asSpell = toObject6

type AsCreaturePlayerPlaneswalker a =
  ToObject3
    a
    'OTCreature
    'OTPlaneswalker
    'OTPlayer

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

sacrifice :: CoPermanent k ot => OPlayer -> [Requirement ot] -> Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo :: (AsPermanent o, CoPermanent k ot) => o -> Card ot -> Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

tapCost :: AsPermanent o => o -> Cost ot
tapCost = TapCost . asPermanent

destroy :: AsPermanent o => o -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility :: AsActivatedOrTriggeredAbility o => o -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell o => o -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

class TypeableOT k ot => CoPermanent k (ot :: k) where
  coPermanent :: WPermanent ot

instance CoPermanent ObjectType1 OTArtifact where
  coPermanent = WPermanentArtifact

instance CoPermanent ObjectType1 OTCreature where
  coPermanent = WPermanentCreature

instance CoPermanent ObjectType1 OTEnchantment where
  coPermanent = WPermanentEnchantment

instance CoPermanent ObjectType1 OTLand where
  coPermanent = WPermanentLand

instance CoPermanent ObjectType1 OTPlaneswalker where
  coPermanent = WPermanentPlaneswalker

instance CoPermanent ObjectType5 OTPermanent where
  coPermanent = WPermanent

instance Inst2 IsPermanentType a b => CoPermanent ObjectType2 '(OT, a, b) where
  coPermanent = WPermanent2 :: WPermanent '(OT, a, b)

instance Inst3 IsPermanentType a b c => CoPermanent ObjectType3 '(OT, a, b, c) where
  coPermanent = WPermanent3 :: WPermanent '(OT, a, b, c)

instance Inst4 IsPermanentType a b c d => CoPermanent ObjectType4 '(OT, a, b, c, d) where
  coPermanent = WPermanent4 :: WPermanent '(OT, a, b, c, d)

class TypeableOT k ot => CoAny k (ot :: k) where
  coAny :: WAny ot

instance CoAny ObjectType1 OTInstant where
  coAny = WAnyInstant

instance CoAny ObjectType1 OTSorcery where
  coAny = WAnySorcery

instance CoAny ObjectType1 OTPlayer where
  coAny = WAnyPlayer

instance CoAny ObjectType1 OTArtifact where
  coAny = WAnyPermanent coPermanent

instance CoAny ObjectType1 OTCreature where
  coAny = WAnyPermanent coPermanent

instance CoAny ObjectType1 OTEnchantment where
  coAny = WAnyPermanent coPermanent

instance CoAny ObjectType1 OTLand where
  coAny = WAnyPermanent coPermanent

instance CoAny ObjectType1 OTPlaneswalker where
  coAny = WAnyPermanent coPermanent

instance CoAny ObjectType5 OTPermanent where
  coAny = WAnyPermanent coPermanent

instance Inst2 IsPermanentType a b => CoAny ObjectType2 '(OT, a, b) where
  coAny = WAnyPermanent coPermanent

instance Inst3 IsPermanentType a b c => CoAny ObjectType3 '(OT, a, b, c) where
  coAny = WAnyPermanent coPermanent

instance Inst4 IsPermanentType a b c d => CoAny ObjectType4 '(OT, a, b, c, d) where
  coAny = WAnyPermanent coPermanent

is :: CoAny k ot => ObjectN ot -> Requirement ot
is = Is coAny

satisfies :: CoAny k ot => ObjectN ot -> [Requirement ot] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent k ot => [Requirement ot] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent k ot => Requirement ot
tapped = Tapped coPermanent

addToBattlefield :: CoPermanent k ot => OPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: ColorsLike c => c -> Requirement ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost ManaCost ot where
  asCost = ManaCost

playerPays :: AsCost c OTPlayer => c -> Requirement OTPlayer
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

colorless :: TypeableOT k ot => Requirement ot
colorless = Not colored
