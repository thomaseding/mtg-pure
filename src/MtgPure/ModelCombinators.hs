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
    AsWithMaskedObject (..),
    AsWithLinkedObject (..),
    AsWithThis (..),
    mkCard,
    mkToken,
    becomesTapped,
    event,
    ifThen,
    ifElse,
    nonBasic,
    tapCost,
    ofColors,
    playerPays,
    addManaAnyColor,
    gain,
    lose,
    is,
    satisfies,
    hasAbility,
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
    untilEndOfTurn,
  )
where

import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Color (Color)
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OArtifact,
    OCreature,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OEnchantment,
    OInstant,
    OLand,
    ON1,
    ON2,
    ON3,
    ON4,
    ON5,
    OPermanent,
    OPlaneswalker,
    OPlayer,
    OSorcery,
    OSpell,
  )
import safe MtgPure.Model.ObjectType
  ( ObjectType (..),
  )
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.Permanent (IsPermanentType, WPermanent (..))
import safe MtgPure.Model.Recursive
  ( Ability,
    Card (..),
    CardTypeDef,
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    EventListener (..),
    Requirement (..),
    Token (Token),
    TypeableOT,
    TypeableOT2,
    WithLinkedObject (..),
    WithMaskedObject (..),
    WithThis (..),
  )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.ToManaPool (ToManaPool (..))
import safe MtgPure.Model.ToObjectN.Classes
  ( ToObject12 (..),
    ToObject2 (..),
    ToObject3 (..),
    ToObject5 (..),
    ToObject6 (..),
    ToObject8 (..),
  )
import safe MtgPure.Model.Tribal (Tribal (..))
import safe MtgPure.Model.Variable (Variable)

class AsWithLinkedObject ot where
  linked :: TypeableOT2 ot x => [Requirement ot] -> (ot -> x ot) -> WithLinkedObject x ot

instance Inst1 IsObjectType a => AsWithLinkedObject (ON1 a) where
  linked = L1

instance Inst2 IsObjectType a b => AsWithLinkedObject (ON2 a b) where
  linked = L2

instance Inst3 IsObjectType a b c => AsWithLinkedObject (ON3 a b c) where
  linked = L3

instance Inst4 IsObjectType a b c d => AsWithLinkedObject (ON4 a b c d) where
  linked = L4

instance Inst5 IsObjectType a b c d e => AsWithLinkedObject (ON5 a b c d e) where
  linked = L5

class AsWithMaskedObject ot' where
  masked :: TypeableOT2 ot x => [Requirement ot'] -> (ot' -> x ot) -> WithMaskedObject x ot

instance Inst1 IsObjectType a => AsWithMaskedObject (ON1 a) where
  masked = M1

instance Inst2 IsObjectType a b => AsWithMaskedObject (ON2 a b) where
  masked = M2

instance Inst3 IsObjectType a b c => AsWithMaskedObject (ON3 a b c) where
  masked = M3

instance Inst4 IsObjectType a b c d => AsWithMaskedObject (ON4 a b c d) where
  masked = M4

instance Inst5 IsObjectType a b c d e => AsWithMaskedObject (ON5 a b c d e) where
  masked = M5

class AsWithThis ot where
  thisObject :: (ot -> x ot) -> WithThis x ot

instance Inst1 IsObjectType a => AsWithThis (ON1 a) where
  thisObject = T1

instance Inst2 IsObjectType a b => AsWithThis (ON2 a b) where
  thisObject = T2

instance Inst3 IsObjectType a b c => AsWithThis (ON3 a b c) where
  thisObject = T3

instance Inst4 IsObjectType a b c d => AsWithThis (ON4 a b c d) where
  thisObject = T4

instance Inst5 IsObjectType a b c d e => AsWithThis (ON5 a b c d e) where
  thisObject = T5

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

spellCost :: ToManaCost a => a -> Elect (Cost ot) ot
spellCost = Cost . ManaCost . toManaCost

noCost :: Elect (Cost ot) ot
noCost = Cost $ OrCosts []

dealDamage ::
  (AsDamageSource source, AsCreaturePlayerPlaneswalker target, AsDamage damage) =>
  source ->
  target ->
  damage ->
  Effect 'OneShot
dealDamage source target = DealDamage (asDamageSource source) (asCreaturePlayerPlaneswalker target) . asDamage

controllerOf :: AsAny o => o -> (OPlayer -> Elect e ot) -> Elect e ot
controllerOf = ControllerOf . asAny

sacrifice :: CoPermanent ot => OPlayer -> [Requirement ot] -> Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo :: (AsPermanent o, CoPermanent ot) => o -> Card ot -> Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

tapCost :: AsPermanent o => o -> Cost ot
tapCost = TapCost . asPermanent

destroy :: AsPermanent o => o -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility :: AsActivatedOrTriggeredAbility o => o -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell o => o -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

class TypeableOT ot => CoPermanent ot where
  coPermanent :: WPermanent ot

instance CoPermanent OArtifact where
  coPermanent = WPermanentArtifact

instance CoPermanent OCreature where
  coPermanent = WPermanentCreature

instance CoPermanent OEnchantment where
  coPermanent = WPermanentEnchantment

instance CoPermanent OLand where
  coPermanent = WPermanentLand

instance CoPermanent OPlaneswalker where
  coPermanent = WPermanentPlaneswalker

instance CoPermanent OPermanent where
  coPermanent = WPermanent

instance Inst2 IsPermanentType a b => CoPermanent (ON2 a b) where
  coPermanent = WPermanent2 :: WPermanent (ON2 a b)

instance Inst3 IsPermanentType a b c => CoPermanent (ON3 a b c) where
  coPermanent = WPermanent3 :: WPermanent (ON3 a b c)

instance Inst4 IsPermanentType a b c d => CoPermanent (ON4 a b c d) where
  coPermanent = WPermanent4 :: WPermanent (ON4 a b c d)

class TypeableOT ot => CoAny ot where
  coAny :: WAny ot

instance CoAny OInstant where
  coAny = WAnyInstant

instance CoAny OSorcery where
  coAny = WAnySorcery

instance CoAny OPlayer where
  coAny = WAnyPlayer

instance CoAny OArtifact where
  coAny = WAnyArtifact

instance CoAny OCreature where
  coAny = WAnyCreature

instance CoAny OEnchantment where
  coAny = WAnyEnchantment

instance CoAny OLand where
  coAny = WAnyLand

instance CoAny OPlaneswalker where
  coAny = WAnyPlaneswalker

instance Inst2 IsPermanentType a b => CoAny (ON2 a b) where
  coAny = WAny2

instance Inst3 IsPermanentType a b c => CoAny (ON3 a b c) where
  coAny = WAny3

instance Inst4 IsPermanentType a b c d => CoAny (ON4 a b c d) where
  coAny = WAny4

instance Inst5 IsPermanentType a b c d e => CoAny (ON5 a b c d e) where
  coAny = WAny5

is :: CoAny ot => ot -> Requirement ot
is = Is coAny

satisfies :: CoAny ot => ot -> [Requirement ot] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent ot => [Requirement ot] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent ot => Requirement ot
tapped = Tapped coPermanent

addToBattlefield :: CoPermanent ot => OPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: ColorsLike c => c -> Requirement ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost ManaCost ot where
  asCost = ManaCost

playerPays :: AsCost c OPlayer => c -> Requirement OPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect ot

instance ElectEffect (Effect e) (Elect (Effect e)) where
  effect = Effect . pure

instance ElectEffect [Effect e] (Elect (Effect e)) where
  effect = Effect

instance ElectEffect (Effect 'Continuous) (Elect (Effect 'OneShot)) where
  effect = Effect . pure . EffectContinuous

event :: EventListener -> Elect EventListener ot
event = Event

class Branchable e ot where
  branchEmpty :: Elect e ot

instance Branchable (Cost ot) ot where
  branchEmpty = Cost $ AndCosts []

instance Branchable EventListener ot where
  branchEmpty = Event $ Events []

instance Branchable (Effect e) ot where
  branchEmpty = Effect []

ifThen :: Branchable e ot => Condition -> Elect e ot -> Elect e ot
ifThen cond elect = If cond elect branchEmpty

ifElse :: Branchable e ot => Condition -> Elect e ot -> Elect e ot
ifElse cond = If cond branchEmpty

nonBasic :: Requirement OLand
nonBasic = RAnd $ map (Not . HasBasicLandType) [minBound ..]

colored :: Requirement ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: TypeableOT ot => Requirement ot
colorless = Not colored

addManaAnyColor :: OPlayer -> Int -> Effect 'OneShot
addManaAnyColor player amount =
  EOr
    [ AddMana player $ toManaPool (W, amount),
      AddMana player $ toManaPool (U, amount),
      AddMana player $ toManaPool (B, amount),
      AddMana player $ toManaPool (R, amount),
      AddMana player $ toManaPool (G, amount)
    ]

class (AsWithThis ot, TypeableOT ot) => MkCard t ot where
  mkCard :: CardName -> (ot -> CardTypeDef t ot) -> Card ot

instance (AsWithThis ot, TypeableOT ot) => MkCard 'NonTribal ot where
  mkCard name = Card name . thisObject

instance (AsWithThis ot, TypeableOT ot) => MkCard 'Tribal ot where
  mkCard name = TribalCard name . thisObject

mkToken :: MkCard tribal ot => CardName -> (ot -> CardTypeDef tribal ot) -> Token ot
mkToken name = Token . mkCard name

hasAbility :: AsWithThis ot => (ot -> Ability ot) -> Requirement ot
hasAbility = HasAbility . thisObject

becomesTapped :: CoPermanent ot => WithLinkedObject (Elect (Effect 'OneShot)) ot -> EventListener
becomesTapped = BecomesTapped coPermanent

untilEndOfTurn :: Elect (Effect 'OneShot) OPlayer -> Effect 'OneShot
untilEndOfTurn = EffectContinuous . Until . event . TimePoint (StepBegin CleanupStep)

gain :: CoAny ot => ot -> Ability ot -> Effect 'Continuous
gain = Gain coAny

lose :: CoAny ot => ot -> Ability ot -> Effect 'Continuous
lose = Lose coAny
