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
  Proxy (Proxy),
  tyAp,
  ToCard (..),
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
  AsWithLinkedObject (..),
  AsWithMaskedObject (..),
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
  isBasic,
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
  nonBlack,
  tapped,
  untilEndOfTurn,
  putOntoBattlefield,
  searchLibrary,
) where

import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.LandType (LandType (BasicLand))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, OT4, OT5, OT6, ObjectType (..))
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
import safe MtgPure.Model.Recursive (
  Ability,
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
  IsOT,
  IsZO,
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
import safe MtgPure.Model.Variable (Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject (
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

instance Typeable ef => CoNonProxy (Elect (Effect ef)) where
  coNonProxy = NonProxyElectEffect

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

class IsZO zone ot => AsWithThis ot zone liftOT ot1s | ot zone liftOT -> ot1s, ot1s -> ot zone where
  thisObject :: (ot1s -> liftOT ot) -> WithThis zone liftOT ot

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
  AsDamageSource ot => ZO 'Battlefield ot -> ZO 'Battlefield OTDamageSource
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
  ( AsDamageSource source
  , AsCreaturePlayerPlaneswalker target
  , AsDamage damage
  ) =>
  ZO 'Battlefield source ->
  ZO 'Battlefield target ->
  damage ->
  Effect 'OneShot
dealDamage source target =
  DealDamage (asDamageSource source) (asCreaturePlayerPlaneswalker target)
    . asDamage

controllerOf ::
  (AsAny ot', IsZO zone ot') => ZO zone ot' -> (OPlayer -> Elect e ot) -> Elect e ot
controllerOf = ControllerOf . asAny

sacrifice ::
  CoPermanent ot =>
  OPlayer ->
  [Requirement 'Battlefield ot] ->
  Effect 'OneShot
sacrifice = Sacrifice coPermanent

changeTo ::
  (AsPermanent ot, CoPermanent ot) =>
  ZO 'Battlefield ot ->
  Card ot ->
  Effect 'Continuous
changeTo = ChangeTo coPermanent . asPermanent

tapCost :: AsPermanent ot => ZO 'Battlefield ot -> Cost ot
tapCost = TapCost . asPermanent

destroy :: AsPermanent ot => ZO 'Battlefield ot -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility ::
  AsActivatedOrTriggeredAbility ot => ZO 'Stack ot -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell ot => ZO 'Stack ot -> Effect 'OneShot
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

sacrificeCost :: CoPermanent ot => [Requirement 'Battlefield ot] -> Cost ot
sacrificeCost = SacrificeCost coPermanent

tapped :: CoPermanent ot => Requirement 'Battlefield ot
tapped = Tapped coPermanent

addToBattlefield :: CoPermanent ot => OPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield coPermanent

ofColors :: ColorsLike c => c -> Requirement zone ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost ManaCost ot where
  asCost = ManaCost

playerPays :: AsCost c OPlayer => c -> Requirement zone OTPlayer
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

ifThenElse ::
  AsIfThenElse e ot => Condition -> Elect e ot -> Elect e ot -> Elect e ot
ifThenElse cond then_ else_ = If cond then_ $ liftElse else_

isBasic :: IsZone zone => Requirement zone OTLand
isBasic = ROr $ map (HasLandType . BasicLand) [minBound ..]

nonBasic :: IsZone zone => Requirement zone OTLand
nonBasic = RAnd $ map (Not . HasLandType . BasicLand) [minBound ..]

nonBlack :: IsZO zone ot => Requirement zone ot
nonBlack = Not $ ofColors Black

colored :: Requirement zone ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: IsZO zone ot => Requirement zone ot
colorless = Not colored

addManaAnyColor :: OPlayer -> Int -> Effect 'OneShot
addManaAnyColor player amount =
  EOr
    [ AddMana player $ toManaPool (W, amount)
    , AddMana player $ toManaPool (U, amount)
    , AddMana player $ toManaPool (B, amount)
    , AddMana player $ toManaPool (R, amount)
    , AddMana player $ toManaPool (G, amount)
    ]

class
  (AsWithThis ot 'Battlefield (CardTypeDef tribal) ot1s) =>
  MkCard tribal ot ot1s
  where
  mkCard :: CardName -> (ot1s -> CardTypeDef tribal ot) -> Card ot

instance
  MkCard
    'NonTribal
    OTArtifactCreature
    (ZO 'Battlefield OTArtifact, ZO 'Battlefield OTCreature)
  where
  mkCard name = Card name coCard . T2

instance
  MkCard
    'NonTribal
    OTEnchantmentCreature
    (ZO 'Battlefield OTCreature, ZO 'Battlefield OTEnchantment)
  where
  mkCard name = Card name coCard . T2

instance
  (CoCard (OT1 a), IsObjectType a) =>
  MkCard 'NonTribal (OT1 a) (ZO 'Battlefield (OT1 a))
  where
  mkCard name = Card name coCard . thisObject

instance
  (CoCard (OT1 a), IsObjectType a) =>
  MkCard 'Tribal (OT1 a) (ZO 'Battlefield (OT1 a))
  where
  mkCard name = TribalCard name coCard . thisObject

-- instance
--   (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
--   MkCard 'NonTribal (OT2 a b) (ZO 'Battlefield (OT1 a), ZO 'Battlefield (OT1 b))
--   where
--   mkCard name = Card name coCard . thisObject

-- instance
--   (CoCard (OT2 a b), Inst2 IsObjectType a b) =>
--   MkCard 'Tribal (OT2 a b) (ZO 'Battlefield (OT1 a), ZO 'Battlefield (OT1 b))
--   where
--   mkCard name = TribalCard name coCard . thisObject

mkToken ::
  (CoPermanent ot, MkCard tribal ot ot1s) =>
  CardName ->
  (ot1s -> CardTypeDef tribal ot) ->
  Token ot
mkToken name = Token coPermanent . mkCard name

hasAbility ::
  (AsWithThis ot zone Ability ot1s, IsZO zone ot) =>
  (ot1s -> Ability ot) ->
  Requirement zone ot
hasAbility = HasAbility . thisObject

becomesTapped ::
  CoPermanent ot =>
  WithLinkedObject 'Battlefield (Elect (Effect 'OneShot)) ot ->
  EventListener
becomesTapped = BecomesTapped coPermanent

untilEndOfTurn :: Effect 'Continuous -> Effect 'OneShot
untilEndOfTurn =
  EffectContinuous . Until (event $ TimePoint (StepBegin CleanupStep) Proxy)

gain :: CoAny ot => ZO 'Battlefield ot -> Ability ot -> Effect 'Continuous
gain = Gain coAny

lose :: CoAny ot => ZO 'Battlefield ot -> Ability ot -> Effect 'Continuous
lose = Lose coAny

class HasLandType a where
  hasLandType :: a -> Requirement zone OTLand

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
  WithLinkedObject 'Library (Elect (Effect 'OneShot)) ot ->
  Effect 'OneShot
searchLibrary = SearchLibrary coCard
