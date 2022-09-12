{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
  ( ToCard (..),
    ToToken (..),
    ToSetCard (..),
    ToSetToken (..),
    AsAny,
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
    HasLandType (..),
    AsIfThen (..),
    AsIfThenElse (..),
    mkCard,
    mkToken,
    becomesTapped,
    event,
    ifThen,
    ifElse,
    ifThenElse,
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
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Color (Color)
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.LandType (LandType (BasicLand))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OLand,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType
  ( OT1,
    OT2,
    OT3,
    OT4,
    OT5,
    ObjectType (..),
  )
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTCard,
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
  ( Ability,
    Card (..),
    CardTypeDef,
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    Else (..),
    Event,
    EventListener,
    EventListener' (..),
    NonProxy (..),
    Requirement (..),
    SetCard (..),
    SetToken (..),
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

class ToCard card where
  toCard :: card -> Card OTCard

instance ToCard (Card OTCard) where
  toCard = id

instance ToCard (Card OTArtifact) where
  toCard = ArtifactCard

instance ToCard (Card OTCreature) where
  toCard = CreatureCard

instance ToCard (Card OTEnchantment) where
  toCard = EnchantmentCard

instance ToCard (Card OTInstant) where
  toCard = InstantCard

instance ToCard (Card OTLand) where
  toCard = LandCard

instance ToCard (Card OTPlaneswalker) where
  toCard = PlaneswalkerCard

instance ToCard (Card OTSorcery) where
  toCard = SorceryCard

class ToSetCard card where
  toSetCard :: card -> SetCard OTCard

instance ToSetCard (SetCard OTCard) where
  toSetCard = id

instance ToSetCard (SetCard OTArtifact) where
  toSetCard (SetCard s r c) = SetCard s r $ ArtifactCard c

instance ToSetCard (SetCard OTCreature) where
  toSetCard (SetCard s r c) = SetCard s r $ CreatureCard c

instance ToSetCard (SetCard OTEnchantment) where
  toSetCard (SetCard s r c) = SetCard s r $ EnchantmentCard c

instance ToSetCard (SetCard OTInstant) where
  toSetCard (SetCard s r c) = SetCard s r $ InstantCard c

instance ToSetCard (SetCard OTLand) where
  toSetCard (SetCard s r c) = SetCard s r $ LandCard c

instance ToSetCard (SetCard OTPlaneswalker) where
  toSetCard (SetCard s r c) = SetCard s r $ PlaneswalkerCard c

instance ToSetCard (SetCard OTSorcery) where
  toSetCard (SetCard s r c) = SetCard s r $ SorceryCard c

class ToToken token where
  toToken :: token -> Token OTCard

instance ToToken (Token OTCard) where
  toToken = id

instance ToToken (Token OTArtifact) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTCreature) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTEnchantment) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTInstant) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTLand) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTPlaneswalker) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTSorcery) where
  toToken (Token x) = Token $ toCard x

class ToSetToken token where
  toSetToken :: token -> SetToken OTCard

instance ToSetToken (SetToken OTCard) where
  toSetToken = id

instance ToSetToken (SetToken OTArtifact) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTCreature) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTEnchantment) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTInstant) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTLand) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTPlaneswalker) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTSorcery) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

class Typeable x => CoNonProxy x where
  coNonProxy :: NonProxy x

instance CoNonProxy (Elect (Effect 'OneShot)) where
  coNonProxy = NonProxyElectEffectOneShot

class TypeableOT2 ot x => AsWithLinkedObject x ot where
  linked :: [Requirement (ObjectN ot)] -> (ObjectN ot -> x ot) -> WithLinkedObject x ot

instance (CoNonProxy x, Inst1 IsObjectType a) => AsWithLinkedObject x (OT1 a) where
  linked = L1 coNonProxy

instance (CoNonProxy x, Inst2 IsObjectType a b) => AsWithLinkedObject x (OT2 a b) where
  linked = L2 coNonProxy

instance (CoNonProxy x, Inst3 IsObjectType a b c) => AsWithLinkedObject x (OT3 a b c) where
  linked = L3 coNonProxy

instance (CoNonProxy x, Inst4 IsObjectType a b c d) => AsWithLinkedObject x (OT4 a b c d) where
  linked = L4 coNonProxy

instance (CoNonProxy x, Inst5 IsObjectType a b c d e) => AsWithLinkedObject x (OT5 a b c d e) where
  linked = L5 coNonProxy

class AsWithMaskedObject ot' where
  masked :: TypeableOT2 ot x => [Requirement (ObjectN ot')] -> (ObjectN ot' -> x ot) -> WithMaskedObject x ot

instance Inst1 IsObjectType a => AsWithMaskedObject (OT1 a) where
  masked = M1

instance Inst2 IsObjectType a b => AsWithMaskedObject (OT2 a b) where
  masked = M2

instance Inst3 IsObjectType a b c => AsWithMaskedObject (OT3 a b c) where
  masked = M3

instance Inst4 IsObjectType a b c d => AsWithMaskedObject (OT4 a b c d) where
  masked = M4

instance Inst5 IsObjectType a b c d e => AsWithMaskedObject (OT5 a b c d e) where
  masked = M5

class AsWithThis ot where
  thisObject :: (ObjectN ot -> x ot) -> WithThis x ot

instance Inst1 IsObjectType a => AsWithThis (OT1 a) where
  thisObject = T1

instance Inst2 IsObjectType a b => AsWithThis (OT2 a b) where
  thisObject = T2

instance Inst3 IsObjectType a b c => AsWithThis (OT3 a b c) where
  thisObject = T3

instance Inst4 IsObjectType a b c d => AsWithThis (OT4 a b c d) where
  thisObject = T4

instance Inst5 IsObjectType a b c d e => AsWithThis (OT5 a b c d e) where
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

sacrifice :: CoPermanent ot => OPlayer -> [Requirement (ObjectN ot)] -> Effect 'OneShot
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

instance Inst2 IsPermanentType a b => CoPermanent (OT2 a b) where
  coPermanent = WPermanent2 :: WPermanent (OT2 a b)

instance Inst3 IsPermanentType a b c => CoPermanent (OT3 a b c) where
  coPermanent = WPermanent3 :: WPermanent (OT3 a b c)

instance Inst4 IsPermanentType a b c d => CoPermanent (OT4 a b c d) where
  coPermanent = WPermanent4 :: WPermanent (OT4 a b c d)

class TypeableOT ot => CoAny ot where
  coAny :: WAny ot

instance CoAny OTInstant where
  coAny = WAnyInstant

instance CoAny OTSorcery where
  coAny = WAnySorcery

instance CoAny OTPlayer where
  coAny = WAnyPlayer

instance CoAny OTArtifact where
  coAny = WAnyArtifact

instance CoAny OTCreature where
  coAny = WAnyCreature

instance CoAny OTEnchantment where
  coAny = WAnyEnchantment

instance CoAny OTLand where
  coAny = WAnyLand

instance CoAny OTPlaneswalker where
  coAny = WAnyPlaneswalker

instance Inst2 IsPermanentType a b => CoAny (OT2 a b) where
  coAny = WAny2

instance Inst3 IsPermanentType a b c => CoAny (OT3 a b c) where
  coAny = WAny3

instance Inst4 IsPermanentType a b c d => CoAny (OT4 a b c d) where
  coAny = WAny4

instance Inst5 IsPermanentType a b c d e => CoAny (OT5 a b c d e) where
  coAny = WAny5

is :: CoAny ot => ObjectN ot -> Requirement (ObjectN ot)
is = Is coAny

satisfies :: CoAny ot => ObjectN ot -> [Requirement (ObjectN ot)] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent ot => [Requirement (ObjectN ot)] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent ot => Requirement (ObjectN ot)
tapped = Tapped coPermanent

addToBattlefield :: CoPermanent ot => OPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: ColorsLike c => c -> Requirement (ObjectN ot)
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

class EventLike x where
  event :: x -> Elect x ot

instance EventLike Event where
  event = Event

instance EventLike EventListener where
  event = Listen

class AsIfThen e ot where
  thenEmpty :: Elect e ot
  elseEmpty :: Else e ot
  default elseEmpty :: AsIfThenElse e ot => Else e ot
  elseEmpty = liftElse thenEmpty

instance AsIfThen (Cost ot) ot where
  thenEmpty = Cost $ AndCosts []

instance AsIfThen (Effect 'OneShot) ot where
  thenEmpty = Effect []

instance AsIfThen EventListener ot where
  thenEmpty = event $ Events []
  elseEmpty = ElseEvent

class AsIfThen e ot => AsIfThenElse e ot where
  liftElse :: Elect e ot -> Else e ot

instance AsIfThenElse (Cost ot) ot where
  liftElse = ElseCost

instance AsIfThenElse (Effect 'OneShot) ot where
  liftElse = ElseEffect

ifThen :: AsIfThen e ot => Condition -> Elect e ot -> Elect e ot
ifThen cond then_ = If cond then_ elseEmpty

ifElse :: AsIfThen e ot => Condition -> Elect e ot -> Elect e ot
ifElse cond else_ = If (CNot cond) else_ elseEmpty

ifThenElse :: AsIfThenElse e ot => Condition -> Elect e ot -> Elect e ot -> Elect e ot
ifThenElse cond then_ else_ = If cond then_ $ liftElse else_

nonBasic :: Requirement OLand
nonBasic = RAnd $ map (Not . HasLandType . BasicLand) [minBound ..]

colored :: Requirement (ObjectN ot)
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: TypeableOT ot => Requirement (ObjectN ot)
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
  mkCard :: CardName -> (ObjectN ot -> CardTypeDef t ot) -> Card ot

instance (AsWithThis ot, TypeableOT ot) => MkCard 'NonTribal ot where
  mkCard name = Card name . thisObject

instance (AsWithThis ot, TypeableOT ot) => MkCard 'Tribal ot where
  mkCard name = TribalCard name . thisObject

mkToken :: MkCard tribal ot => CardName -> (ObjectN ot -> CardTypeDef tribal ot) -> Token ot
mkToken name = Token . mkCard name

hasAbility :: AsWithThis ot => (ObjectN ot -> Ability ot) -> Requirement (ObjectN ot)
hasAbility = HasAbility . thisObject

becomesTapped :: CoPermanent ot => WithLinkedObject (Elect (Effect 'OneShot)) ot -> EventListener
becomesTapped = BecomesTapped coPermanent

untilEndOfTurn :: Effect 'Continuous -> Effect 'OneShot
untilEndOfTurn = EffectContinuous . Until (event $ TimePoint (StepBegin CleanupStep) Proxy)

gain :: CoAny ot => ObjectN ot -> Ability ot -> Effect 'Continuous
gain = Gain coAny

lose :: CoAny ot => ObjectN ot -> Ability ot -> Effect 'Continuous
lose = Lose coAny

class HasLandType a where
  hasLandType :: a -> Requirement OLand

instance HasLandType BasicLandType where
  hasLandType = HasLandType . BasicLand

instance HasLandType LandType where
  hasLandType = HasLandType
