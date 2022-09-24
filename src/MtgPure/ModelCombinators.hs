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
  activated,
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
  lose,
  mkCard,
  mkToken,
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
  tapped,
  ToCard (..),
  ToSetCard (..),
  ToSetToken (..),
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
import safe MtgPure.Model.CardName (CardName)
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
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.Card (IsCardType, WCard (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTArtifact,
  OTArtifactCreature,
  OTCard,
  OTCreature,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTLand,
  OTPermanent,
  OTPlaneswalker,
  OTPlayer,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.ObjectType.Permanent (IsPermanentType, WPermanent (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  Card (..),
  CardTypeDef,
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectPrePost,
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  NonProxy (..),
  Requirement (..),
  SetCard (..),
  SetToken (..),
  Token (..),
  WithLinkedObject (..),
  WithMaskedObject (..),
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
  ToObject5,
  ToObject6,
  ToObject8,
 )
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Tribal (Tribal (..))
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject (
  IsOT,
  IsZO,
  OPlayer,
  ZO,
  toZO12,
  toZO2,
  toZO3,
  toZO5,
  toZO6,
  toZO8,
 )

tyAp :: forall a f. f a -> f a
tyAp = id

class ToCard card where
  toCard :: card -> Card ()

instance ToCard (Card ()) where
  toCard = id

instance ToCard (Card OTArtifact) where
  toCard = ArtifactCard

instance ToCard (Card OTArtifactCreature) where
  toCard = ArtifactCreatureCard

instance ToCard (Card OTCreature) where
  toCard = CreatureCard

instance ToCard (Card OTEnchantment) where
  toCard = EnchantmentCard

instance ToCard (Card OTEnchantmentCreature) where
  toCard = EnchantmentCreatureCard

instance ToCard (Card OTInstant) where
  toCard = InstantCard

instance ToCard (Card OTLand) where
  toCard = LandCard

instance ToCard (Card OTPlaneswalker) where
  toCard = PlaneswalkerCard

instance ToCard (Card OTSorcery) where
  toCard = SorceryCard

class ToSetCard card where
  toSetCard :: card -> SetCard ()

instance ToSetCard (SetCard ()) where
  toSetCard = id

instance ToSetCard (SetCard OTArtifact) where
  toSetCard (SetCard s r c) = SetCard s r $ ArtifactCard c

instance ToSetCard (SetCard OTArtifactCreature) where
  toSetCard (SetCard s r c) = SetCard s r $ ArtifactCreatureCard c

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
  toToken :: token -> Token ()

instance ToToken (Token ()) where
  toToken = id

instance ToToken (Token OTArtifact) where
  toToken (Token _ x) = ArtifactToken $ Token coPermanent x

instance ToToken (Token OTArtifactCreature) where
  toToken (Token _ x) = ArtifactCreatureToken $ Token coPermanent x

instance ToToken (Token OTCreature) where
  toToken (Token _ x) = CreatureToken $ Token coPermanent x

instance ToToken (Token OTEnchantment) where
  toToken (Token _ x) = EnchantmentToken $ Token coPermanent x

instance ToToken (Token OTLand) where
  toToken (Token _ x) = LandToken $ Token coPermanent x

instance ToToken (Token OTPlaneswalker) where
  toToken (Token _ x) = PlaneswalkerToken $ Token coPermanent x

class ToSetToken token where
  toSetToken :: token -> SetToken ()

instance ToSetToken (SetToken ()) where
  toSetToken = id

instance ToSetToken (SetToken OTArtifact) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ ArtifactToken $ Token coPermanent x

instance ToSetToken (SetToken OTArtifactCreature) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ ArtifactCreatureToken $ Token coPermanent x

instance ToSetToken (SetToken OTCreature) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ CreatureToken $ Token coPermanent x

instance ToSetToken (SetToken OTEnchantment) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ EnchantmentToken $ Token coPermanent x

instance ToSetToken (SetToken OTLand) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ LandToken $ Token coPermanent x

instance ToSetToken (SetToken OTPlaneswalker) where
  toSetToken (SetToken s r (Token _ x)) =
    SetToken s r $ PlaneswalkerToken $ Token coPermanent x

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

asDamageSource ::
  AsDamageSource ot => ZO 'ZBattlefield ot -> ZO 'ZBattlefield OTDamageSource
asDamageSource = toZO8

type AsPermanent ot =
  ToObject5
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTLand
    'OTPlaneswalker

asPermanent :: AsPermanent ot => ZO zone ot -> ZO zone OTPermanent
asPermanent = toZO5

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
  ) =>
  ZO 'ZBattlefield source ->
  ZO 'ZBattlefield target ->
  damage ->
  Effect 'OneShot
dealDamage source target =
  DealDamage (asDamageSource source) (asCreaturePlayerPlaneswalker target)
    . asDamage

controllerOf ::
  (AsAny ot', IsZO zone ot') => ZO zone ot' -> (OPlayer -> Elect p e ot) -> Elect p e ot
controllerOf = ControllerOf . asAny

sacrifice ::
  CoPermanent ot =>
  OPlayer ->
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

class IsOT ot => CoCard ot where
  coCard :: WCard ot

instance CoCard OTArtifact where
  coCard = WCardArtifact

instance CoCard OTCreature where
  coCard = WCardCreature

instance CoCard OTEnchantment where
  coCard = WCardEnchantment

instance CoCard OTInstant where
  coCard = WCardInstant

instance CoCard OTLand where
  coCard = WCardLand

instance CoCard OTPlaneswalker where
  coCard = WCardPlaneswalker

instance CoCard OTSorcery where
  coCard = WCardSorcery

instance CoCard OTCard where
  coCard = WCard

instance Inst2 IsCardType a b => CoCard (OT2 a b) where
  coCard = WCard2 :: WCard (OT2 a b)

instance Inst3 IsCardType a b c => CoCard (OT3 a b c) where
  coCard = WCard3 :: WCard (OT3 a b c)

class IsOT ot => CoPermanent ot where
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

class IsOT ot => CoAny ot where
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

is :: (IsZone zone, CoAny ot) => ZO zone ot -> Requirement zone ot
is = Is coAny

satisfies ::
  (IsZone zone, CoAny ot) => ZO zone ot -> [Requirement zone ot] -> Condition
satisfies = Satisfies coAny

sacrificeCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
tapCost = TapCost coPermanent

tapped :: CoPermanent ot => Requirement 'ZBattlefield ot
tapped = IsTapped coPermanent

addToBattlefield :: CoPermanent ot => OPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: (IsZO zone ot, ColorsLike c) => c -> Requirement zone ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost (ManaCost 'Var) ot where
  asCost = ManaCost

playerPays :: (IsZone zone, AsCost c OPlayer) => c -> Requirement zone OTPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect ot

instance ElectEffect (Effect e) (Elect 'Post (Effect e)) where
  effect = Effect . pure

instance ElectEffect [Effect e] (Elect 'Post (Effect e)) where
  effect = Effect

instance ElectEffect (Effect 'Continuous) (Elect 'Post (Effect 'OneShot)) where
  effect = Effect . pure . EffectContinuous

class EventLike el where
  event :: el -> Elect 'Post el ot

instance EventLike Event where
  event = Event

instance EventLike EventListener where
  event = Listen

class AsIfThen (p :: PrePost) (el :: Type) (ot :: Type) where
  thenEmpty :: Elect p el ot

class AsIfThen p el ot => AsIfElse p el ot where
  elseEmpty :: Else p el ot
  default elseEmpty :: AsIfThenElse p el ot => Else p el ot
  elseEmpty = liftElse thenEmpty

class AsIfThen (p :: PrePost) (el :: Type) (ot :: Type) => AsIfThenElse p el ot where
  liftElse :: Elect p el ot -> Else p el ot

instance AsIfThen 'Post (Cost ot) ot where
  thenEmpty = Cost $ AndCosts []

instance AsIfElse 'Post (Cost ot) ot

instance AsIfThen 'Post (Effect 'OneShot) ot where
  thenEmpty = Effect []

instance AsIfElse 'Post (Effect 'OneShot) ot

instance AsIfThen 'Post EventListener ot where
  thenEmpty = event $ Events []

instance AsIfElse 'Post EventListener ot where
  elseEmpty = ElseEvent

instance AsIfThenElse 'Post (Cost ot) ot where
  liftElse = ElseCost

instance AsIfThenElse 'Post (Effect 'OneShot) ot where
  liftElse = ElseEffect

ifThen :: AsIfElse p el ot => Condition -> Elect p el ot -> Elect p el ot
ifThen cond then_ = If cond then_ elseEmpty

ifElse :: AsIfElse p el ot => Condition -> Elect p el ot -> Elect p el ot
ifElse cond else_ = If (CNot cond) else_ elseEmpty

ifThenElse ::
  AsIfThenElse p el ot => Condition -> Elect p el ot -> Elect p el ot -> Elect p el ot
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

addManaAnyColor :: Variable Color -> OPlayer -> Int -> Effect 'OneShot
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

class
  (AsWithThis ot 'ZBattlefield (CardTypeDef tribal) ots) =>
  MkCard tribal ot ots
  where
  mkCard :: CardName -> (ots -> Elect 'Pre (CardTypeDef tribal ot) ot) -> Card ot

instance
  (CoCard (OT1 a), IsObjectType a) =>
  MkCard 'NonTribal (OT1 a) (ZO 'ZBattlefield (OT1 a))
  where
  mkCard name = Card name coCard . thisObject

instance
  (CoCard (OT1 a), IsObjectType a) =>
  MkCard 'Tribal (OT1 a) (ZO 'ZBattlefield (OT1 a))
  where
  mkCard name = TribalCard name coCard . thisObject

instance
  (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
  MkCard 'NonTribal (OT2 a b) (ZO 'ZBattlefield (OT1 a), ZO 'ZBattlefield (OT1 b))
  where
  mkCard name = Card name coCard . thisObject

instance
  (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
  MkCard 'Tribal (OT2 a b) (ZO 'ZBattlefield (OT1 a), ZO 'ZBattlefield (OT1 b))
  where
  mkCard name = TribalCard name coCard . thisObject

mkToken ::
  (CoPermanent ot, MkCard tribal ot ots) =>
  CardName ->
  (ots -> Elect 'Pre (CardTypeDef tribal ot) ot) ->
  Token ot
mkToken name = Token coPermanent . mkCard name

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
  (IsZone zone, CoPermanent ot) => OPlayer -> ZO zone ot -> Effect 'OneShot
putOntoBattlefield = PutOntoBattlefield coPermanent

searchLibrary ::
  CoCard ot =>
  OPlayer ->
  WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot ->
  Effect 'OneShot
searchLibrary = SearchLibrary coCard

activated :: IsOT ot => ElectPrePost (Cost ot) ot -> ElectPrePost (Effect 'OneShot) ot -> Ability ot
activated cost = Activated . Ability cost
