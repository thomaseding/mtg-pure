{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.ModelCombinators (
  addManaAnyColor,
  addToBattlefield,
  asActivatedOrTriggeredAbility,
  AsActivatedOrTriggeredAbility,
  asAny,
  AsAny,
  AsCost (..),
  asCreaturePlayerPlaneswalker,
  AsCreaturePlayerPlaneswalker,
  AsDamage (..),
  AsIfThen (..),
  AsIfThenElse (..),
  asPermanent,
  AsPermanent,
  asSpell,
  AsSpell,
  AsWithLinkedObject (..),
  AsWithMaskedObject (..),
  AsWithMaskedObjects (..),
  AsWithThis (..),
  becomesTapped,
  changeTo,
  CoAny (..),
  colored,
  colorless,
  CoNonProxy (..),
  controllerOf,
  CoPermanent (..),
  counterAbility,
  counterSpell,
  dealDamage,
  destroy,
  ElectEffect (..),
  event,
  gain,
  hasAbility,
  HasLandType (..),
  ifElse,
  ifThen,
  ifThenElse,
  is,
  isBasic,
  isTapped,
  lose,
  -- mkCard,
  -- mkToken,
  noCost,
  nonBasic,
  nonBlack,
  ofColors,
  playerPays,
  Proxy (Proxy),
  putOntoBattlefield,
  sacrifice,
  sacrificeCost,
  satisfies,
  searchLibrary,
  spellCost,
  tapCost,
  ToCard (..),
  ToToken (..),
  tyAp,
  untilEndOfTurn,
) where

import safe Data.Inst (
  Inst1,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
 )
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.LandType (LandType (BasicLand))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object (
  IsObjectType,
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  ObjectType (..),
 )
import safe MtgPure.Model.ObjectType.Any (CoAny (..))
import safe MtgPure.Model.ObjectType.Card (CoCard (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTLand,
  OTPlayer,
  OTSpell,
 )
import safe MtgPure.Model.ObjectType.Permanent (CoPermanent (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  IsSpecificCard,
  List,
  NonProxy (..),
  Requirement (..),
  Token (..),
  WithLinkedObject (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.ToManaPool (ToManaPool (..))
import safe MtgPure.Model.ToObjectN.Classes (
  ToObject12,
  ToObject2,
  ToObject3,
  ToObject6,
  ToObject8,
 )
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject (
  IsOT,
  IsZO,
  ZO,
  ZOPlayer,
 )
import safe MtgPure.Model.ZoneObject.Convert (
  AsPermanent,
  asPermanent,
  toZO12,
  toZO2,
  toZO3,
  toZO6,
  toZO8,
 )

tyAp :: forall a f. f a -> f a
tyAp = id

class ToCard card where
  toCard :: card -> AnyCard

instance ToCard AnyCard where
  toCard = id

instance IsSpecificCard ot => ToCard (Card ot) where
  toCard = AnyCard

class ToToken token where
  toToken :: token -> AnyToken

instance ToToken AnyToken where
  toToken = id

instance CoPermanent ot => ToToken (Token ot) where
  toToken (Token _ x) = AnyToken $ Token coPermanent x

class Typeable x => CoNonProxy x where
  coNonProxy :: NonProxy x

instance (Typeable p, Typeable ef) => CoNonProxy (Elect p (Effect ef)) where
  coNonProxy = NonProxyElectEffect

instance (Typeable ot) => CoNonProxy (Elect 'Pre (Elect 'Post (Effect 'Continuous) ot)) where
  coNonProxy = NonProxyElectPrePostEffect

class (IsOT ot, Typeable liftOT) => AsWithLinkedObject ot zone liftOT where
  linked :: [Requirement zone ot] -> (ZO zone ot -> liftOT ot) -> WithLinkedObject zone liftOT ot

instance (CoNonProxy x, Inst1 IsObjectType a) => AsWithLinkedObject (OT1 a) zone x where
  linked = L1 coNonProxy

instance (CoNonProxy x, Inst2 IsObjectType a b) => AsWithLinkedObject (OT2 a b) zone x where
  linked = L2 coNonProxy

instance (CoNonProxy x, Inst3 IsObjectType a b c) => AsWithLinkedObject (OT3 a b c) zone x where
  linked = L3 coNonProxy

instance (CoNonProxy x, Inst4 IsObjectType a b c d) => AsWithLinkedObject (OT4 a b c d) zone x where
  linked = L4 coNonProxy

instance (CoNonProxy x, Inst5 IsObjectType a b c d e) => AsWithLinkedObject (OT5 a b c d e) zone x where
  linked = L5 coNonProxy

class AsWithMaskedObject ot where
  masked :: forall zone z. Typeable z => [Requirement zone ot] -> (ZO zone ot -> z) -> WithMaskedObject zone z

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

instance Inst6 IsObjectType a b c d e f => AsWithMaskedObject (OT6 a b c d e f) where
  masked = M6

class AsWithMaskedObjects ot where
  maskeds :: forall zone z. Typeable z => [Requirement zone ot] -> (List (ZO zone ot) -> z) -> WithMaskedObjects zone z

instance Inst1 IsObjectType a => AsWithMaskedObjects (OT1 a) where
  maskeds = M1s

instance Inst2 IsObjectType a b => AsWithMaskedObjects (OT2 a b) where
  maskeds = M2s

instance Inst3 IsObjectType a b c => AsWithMaskedObjects (OT3 a b c) where
  maskeds = M3s

instance Inst4 IsObjectType a b c d => AsWithMaskedObjects (OT4 a b c d) where
  maskeds = M4s

instance Inst5 IsObjectType a b c d e => AsWithMaskedObjects (OT5 a b c d e) where
  maskeds = M5s

instance Inst6 IsObjectType a b c d e f => AsWithMaskedObjects (OT6 a b c d e f) where
  maskeds = M6s

class IsZO zone ot => AsWithThis ot zone liftOT ots | ot zone liftOT -> ots, ots -> ot zone where
  thisObject :: (ots -> liftOT ot) -> WithThis zone liftOT ot

instance (IsZO zone (OT1 a), Inst1 IsObjectType a) => AsWithThis (OT1 a) zone liftOT (ZO zone (OT1 a)) where
  thisObject = T1

instance (IsZO zone (OT2 a b), Inst2 IsObjectType a b) => AsWithThis (OT2 a b) zone liftOT (ZO zone (OT1 a), ZO zone (OT1 b)) where
  thisObject = T2

type AsActivatedOrTriggeredAbility ot =
  ToObject2 ot 'OTActivatedAbility 'OTTriggeredAbility

asActivatedOrTriggeredAbility ::
  AsActivatedOrTriggeredAbility ot =>
  ZO zone ot ->
  ZO zone OTActivatedOrTriggeredAbility
asActivatedOrTriggeredAbility = toZO2

type AsAny ot =
  ToObject12
    ot
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

asAny :: AsAny ot => ZO zone ot -> ZO zone OTAny
asAny = toZO12

type AsDamageSource ot =
  ToObject8
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery

asDamageSource :: AsDamageSource ot => ZO zone ot -> ZO zone OTDamageSource
asDamageSource = toZO8

type AsSpell ot =
  ToObject6
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTPlaneswalker
    'OTSorcery

asSpell :: AsSpell ot => ZO zone ot -> ZO zone OTSpell
asSpell = toZO6

type AsCreaturePlayerPlaneswalker ot =
  ToObject3 ot 'OTCreature 'OTPlaneswalker 'OTPlayer

asCreaturePlayerPlaneswalker ::
  AsCreaturePlayerPlaneswalker ot =>
  ZO zone ot ->
  ZO zone OTCreaturePlayerPlaneswalker
asCreaturePlayerPlaneswalker = toZO3

class AsDamage a where
  asDamage :: a -> Damage 'Var

instance AsDamage Integer where
  asDamage n = asDamage (fromInteger n :: Int)

instance AsDamage Int where
  asDamage = Damage

instance AsDamage (Damage 'Var) where
  asDamage = id

instance AsDamage (Variable Int) where
  asDamage = VariableDamage

spellCost :: ToManaCost a => a -> Cost ot
spellCost = ManaCost . toManaCost

noCost :: Cost ot
noCost = OrCosts []

dealDamage ::
  ( AsDamageSource source
  , AsCreaturePlayerPlaneswalker target
  , AsDamage damage
  , IsZO zone OTDamageSource
  ) =>
  ZO zone source ->
  ZO 'ZBattlefield target ->
  damage ->
  Effect 'OneShot
dealDamage source target =
  DealDamage
    (asDamageSource source)
    (asCreaturePlayerPlaneswalker target)
    . asDamage

controllerOf ::
  (AsAny ot', IsZO zone ot') => ZO zone ot' -> (ZOPlayer -> Elect p e ot) -> Elect p e ot
controllerOf = ControllerOf . asAny

sacrifice ::
  CoPermanent ot =>
  ZOPlayer ->
  [Requirement 'ZBattlefield ot] ->
  Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo ::
  (AsPermanent ot, CoPermanent ot) =>
  ZO 'ZBattlefield ot ->
  Card ot ->
  Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

destroy :: AsPermanent ot => ZO 'ZBattlefield ot -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility ::
  AsActivatedOrTriggeredAbility ot => ZO 'ZStack ot -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell ot => ZO 'ZStack ot -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

is :: (IsZone zone, CoAny ot) => ZO zone ot -> Requirement zone ot
is = Is coAny

satisfies ::
  (IsZone zone, CoAny ot) => ZO zone ot -> [Requirement zone ot] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
tapCost = TapCost

isTapped :: CoPermanent ot => Requirement 'ZBattlefield ot
isTapped = IsTapped coPermanent

addToBattlefield :: CoPermanent ot => ZOPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: (IsZO zone ot, ColorsLike c) => c -> Requirement zone ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost (ManaCost 'Var) ot where
  asCost = ManaCost

playerPays :: (IsZone zone, AsCost c OTPlayer) => c -> Requirement zone OTPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect ot

instance Typeable ef => ElectEffect (Effect ef) (Elect 'Post (Effect ef)) where
  effect = Effect . pure

instance Typeable ef => ElectEffect [Effect ef] (Elect 'Post (Effect ef)) where
  effect = Effect

instance ElectEffect (Effect 'Continuous) (Elect 'Post (Effect 'OneShot)) where
  effect = Effect . pure . EffectContinuous

class EventLike el where
  event :: el -> Elect 'Post el ot

instance EventLike Event where
  event = Event

instance EventLike EventListener where
  event = Listen

class AsIfThen (el :: Type) (ot :: Type) where
  thenEmpty :: Elect 'Post el ot

class AsIfThen el ot => AsIfElse el ot where
  elseEmpty :: Else el ot
  default elseEmpty :: AsIfThenElse el ot => Else el ot
  elseEmpty = liftElse thenEmpty

class AsIfThen (el :: Type) (ot :: Type) => AsIfThenElse el ot where
  liftElse :: Elect 'Post el ot -> Else el ot

instance AsIfThen (Effect 'OneShot) ot where
  thenEmpty = Effect []

instance AsIfElse (Effect 'OneShot) ot

instance AsIfThen EventListener ot where
  thenEmpty = event $ Events []

instance AsIfElse EventListener ot where
  elseEmpty = ElseEvent

instance AsIfThenElse (Effect 'OneShot) ot where
  liftElse = ElseEffect

ifThen :: AsIfElse el ot => Condition -> Elect 'Post el ot -> Elect 'Post el ot
ifThen cond then_ = If cond then_ elseEmpty

ifElse :: AsIfElse el ot => Condition -> Elect 'Post el ot -> Elect 'Post el ot
ifElse cond else_ = If (CNot cond) else_ elseEmpty

ifThenElse ::
  AsIfThenElse el ot => Condition -> Elect 'Post el ot -> Elect 'Post el ot -> Elect 'Post el ot
ifThenElse cond then_ else_ = If cond then_ $ liftElse else_

isBasic :: IsZone zone => Requirement zone OTLand
isBasic = ROr $ map (HasLandType . BasicLand) [minBound ..]

nonBasic :: IsZone zone => Requirement zone OTLand
nonBasic = RAnd $ map (Not . HasLandType . BasicLand) [minBound ..]

nonBlack :: IsZO zone ot => Requirement zone ot
nonBlack = Not $ ofColors Black

colored :: IsZO zone ot => Requirement zone ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: IsZO zone ot => Requirement zone ot
colorless = Not colored

addManaAnyColor :: Variable Color -> ZOPlayer -> Int -> Effect 'OneShot
addManaAnyColor color player amount =
  EffectCase $
    CaseColor
      { caseColor = color
      , ofWhite = AddMana player $ toManaPool (W, amount)
      , ofBlue = AddMana player $ toManaPool (U, amount)
      , ofBlack = AddMana player $ toManaPool (B, amount)
      , ofRed = AddMana player $ toManaPool (R, amount)
      , ofGreen = AddMana player $ toManaPool (G, amount)
      }

-- class
--   (AsWithThis ot 'ZBattlefield (CardTypeDef tribal) ots) =>
--   MkCard tribal ot ots
--   where
--   mkCard :: CardName -> (ots -> Elect 'Pre (CardTypeDef tribal ot) ot) -> Card ot

-- instance
--   (CoCard (OT1 a), IsObjectType a) =>
--   MkCard 'NonTribal (OT1 a) (ZO 'ZBattlefield (OT1 a))
--   where
--   mkCard name = Card name coCard . thisObject

-- instance
--   (CoCard (OT1 a), IsObjectType a) =>
--   MkCard 'Tribal (OT1 a) (ZO 'ZBattlefield (OT1 a))
--   where
--   mkCard name = TribalCard name coCard . thisObject

-- instance
--   (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
--   MkCard 'NonTribal (OT2 a b) (ZO 'ZBattlefield (OT1 a), ZO 'ZBattlefield (OT1 b))
--   where
--   mkCard name = Card name coCard . thisObject

-- instance
--   (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
--   MkCard 'Tribal (OT2 a b) (ZO 'ZBattlefield (OT1 a), ZO 'ZBattlefield (OT1 b))
--   where
--   mkCard name = TribalCard name coCard . thisObject

-- mkToken ::
--   (CoPermanent ot, MkCard tribal ot ots) =>
--   CardName ->
--   (ots -> Elect 'Pre (CardTypeDef tribal ot) ot) ->
--   Token ot
-- mkToken name = Token coPermanent . mkCard name

hasAbility ::
  (AsWithThis ot zone Ability ots, IsZO zone ot) =>
  (ots -> Ability ot) ->
  Requirement zone ot
hasAbility = HasAbility . thisObject

becomesTapped ::
  CoPermanent ot =>
  WithLinkedObject 'ZBattlefield (Elect 'Post (Effect 'OneShot)) ot ->
  EventListener
becomesTapped = BecomesTapped coPermanent

untilEndOfTurn :: Effect 'Continuous -> Effect 'OneShot
untilEndOfTurn =
  EffectContinuous . Until (event $ TimePoint (StepBegin CleanupStep) Proxy)

gain :: CoAny ot => ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
gain = Gain coAny

lose :: CoAny ot => ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
lose = Lose coAny

class HasLandType a where
  hasLandType :: IsZone zone => a -> Requirement zone OTLand

instance HasLandType BasicLandType where
  hasLandType = HasLandType . BasicLand

instance HasLandType LandType where
  hasLandType = HasLandType

putOntoBattlefield ::
  (IsZone zone, CoPermanent ot) => ZOPlayer -> ZO zone ot -> Effect 'OneShot
putOntoBattlefield = PutOntoBattlefield coPermanent

searchLibrary ::
  CoCard ot =>
  ZOPlayer ->
  WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot ->
  Effect 'OneShot
searchLibrary = SearchLibrary coCard
