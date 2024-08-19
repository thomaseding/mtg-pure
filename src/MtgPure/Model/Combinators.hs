{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use camelCase" #-}

module MtgPure.Model.Combinators (
  activated,
  activated',
  addManaAnyColor,
  addToBattlefield,
  AsCost (..),
  AsDamage (..),
  AsIfThen (..),
  AsIfThenElse (..),
  AsWithLinkedObject (..),
  AsWithMaskedObject (..),
  AsWithMaskedObjects (..),
  AsWithThis (..),
  becomesTapped,
  CanHaveTrivialManaAbility,
  changeTo,
  chooseAnyColor,
  colored,
  colorless,
  controllerOf,
  counterAbility,
  counterSpell,
  dealDamage,
  destroy,
  didNotPayCost,
  ElectEffect (..),
  event,
  gainAbility,
  gainControl,
  hasAbility,
  HasLandType (..),
  ifElse,
  ifThen,
  ifThenElse,
  is,
  isBasic,
  isTapped,
  loseAbility,
  -- mkCard,
  -- mkToken,
  noCost,
  nonBasic,
  nonBlack,
  ofColors,
  paidCost,
  Proxy (Proxy),
  putOntoBattlefield,
  sacrifice,
  sacrificeCost,
  satisfies,
  searchLibrary,
  manaCost,
  static_,
  static,
  static',
  swampwalk,
  tapCost,
  ToCard (..),
  ToToken (..),
  ToHybrid (..),
  triggered,
  triggered',
  trivialManaAbilities,
  trivialManaAbility,
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
  Inst7,
 )
import safe Data.Kind (Type)
import safe Data.Nat (Fin, NatList (..), ToNat)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (CoNonIntrinsicStage, ElectStage (..))
import safe MtgPure.Model.LandType (LandType (BasicLand))
import safe MtgPure.Model.Mana.ManaCost (ManaCost)
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Mana.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.Mana.ToManaPool (ToManaPool (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.LitOTN (LitOTN (litOTN))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OTN (..),
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNCreature,
  OTNDamageSource,
  OTNLand,
 )
import safe MtgPure.Model.Object.Singleton.Any (CoAny (..))
import safe MtgPure.Model.Object.Singleton.Card (CoCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (..),
  ElectTargetedEffect,
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  List,
  Requirement (..),
  SomeZone (..),
  StaticAbility (Landwalk),
  Token (..),
  TriggeredAbility,
  WithLinkedObject (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisAbility (..),
  WithThisActivated,
  pattern CTrue,
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (
  AsActivatedOrTriggeredAbility,
  AsAny,
  AsCreaturePlayerPlaneswalker,
  AsDamageSource,
  AsPermanent,
  AsSpell,
  asActivatedOrTriggeredAbility,
  asAny,
  asCreaturePlayerPlaneswalker,
  asDamageSource,
  asPermanent,
  asSpell,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZOPlayer,
 )

tyAp :: forall a f. f a -> f a
tyAp = id

class ToCard card where
  toCard :: card -> AnyCard

instance ToCard AnyCard where
  toCard :: AnyCard -> AnyCard
  toCard = id

instance ToCard (Card ot) where
  toCard :: Card ot -> AnyCard
  toCard card = case card of
    Card{} -> AnyCard1 card
    DoubleSidedCard{} -> AnyCard2 card
    SplitCard{} -> AnyCard2 card

class ToToken token where
  toToken :: token -> AnyToken

instance ToToken AnyToken where
  toToken :: AnyToken -> AnyToken
  toToken = id

instance (CoPermanent ot) => ToToken (Token ot) where
  toToken :: (CoPermanent ot) => Token ot -> AnyToken
  toToken (Token x) = AnyToken $ Token x

class (IsOTN ot, Typeable liftOT) => AsWithLinkedObject ot zone liftOT where
  linked :: [Requirement zone ot] -> (ZO zone ot -> liftOT ot) -> WithLinkedObject liftOT zone ot

instance (Typeable x, Inst1 IsObjectType a) => AsWithLinkedObject (OT1 a) zone x where
  linked :: (Typeable x, Inst1 IsObjectType a) => [Requirement zone (OT1 a)] -> (ZO zone (OT1 a) -> x (OT1 a)) -> WithLinkedObject x zone (OT1 a)
  linked = Linked1

instance (Typeable x, Inst2 IsObjectType a b) => AsWithLinkedObject (OT2 a b) zone x where
  linked :: (Typeable x, Inst2 IsObjectType a b) => [Requirement zone (OT2 a b)] -> (ZO zone (OT2 a b) -> x (OT2 a b)) -> WithLinkedObject x zone (OT2 a b)
  linked = Linked2

instance (Typeable x, Inst3 IsObjectType a b c) => AsWithLinkedObject (OT3 a b c) zone x where
  linked :: (Typeable x, Inst3 IsObjectType a b c) => [Requirement zone (OT3 a b c)] -> (ZO zone (OT3 a b c) -> x (OT3 a b c)) -> WithLinkedObject x zone (OT3 a b c)
  linked = Linked3

instance (Typeable x, Inst4 IsObjectType a b c d) => AsWithLinkedObject (OT4 a b c d) zone x where
  linked :: (Typeable x, Inst4 IsObjectType a b c d) => [Requirement zone (OT4 a b c d)] -> (ZO zone (OT4 a b c d) -> x (OT4 a b c d)) -> WithLinkedObject x zone (OT4 a b c d)
  linked = Linked4

instance (Typeable x, Inst5 IsObjectType a b c d e) => AsWithLinkedObject (OT5 a b c d e) zone x where
  linked :: (Typeable x, Inst5 IsObjectType a b c d e) => [Requirement zone (OT5 a b c d e)] -> (ZO zone (OT5 a b c d e) -> x (OT5 a b c d e)) -> WithLinkedObject x zone (OT5 a b c d e)
  linked = Linked5

class AsWithMaskedObject ot where
  masked ::
    forall zone liftOT ot'.
    (Typeable (liftOT ot')) =>
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot') ->
    WithMaskedObject liftOT zone ot'

instance (Inst1 IsObjectType a) => AsWithMaskedObject (OT1 a) where
  masked :: (Inst1 IsObjectType a, Typeable (liftOT ot')) => [Requirement zone (OT1 a)] -> (ZO zone (OT1 a) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked1

instance (Inst2 IsObjectType a b) => AsWithMaskedObject (OT2 a b) where
  masked :: (Inst2 IsObjectType a b, Typeable (liftOT ot')) => [Requirement zone (OT2 a b)] -> (ZO zone (OT2 a b) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked2

instance (Inst3 IsObjectType a b c) => AsWithMaskedObject (OT3 a b c) where
  masked :: (Inst3 IsObjectType a b c, Typeable (liftOT ot')) => [Requirement zone (OT3 a b c)] -> (ZO zone (OT3 a b c) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked3

instance (Inst4 IsObjectType a b c d) => AsWithMaskedObject (OT4 a b c d) where
  masked :: (Inst4 IsObjectType a b c d, Typeable (liftOT ot')) => [Requirement zone (OT4 a b c d)] -> (ZO zone (OT4 a b c d) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked4

instance (Inst5 IsObjectType a b c d e) => AsWithMaskedObject (OT5 a b c d e) where
  masked :: (Inst5 IsObjectType a b c d e, Typeable (liftOT ot')) => [Requirement zone (OT5 a b c d e)] -> (ZO zone (OT5 a b c d e) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked5

instance (Inst6 IsObjectType a b c d e f) => AsWithMaskedObject (OT6 a b c d e f) where
  masked :: (Inst6 IsObjectType a b c d e f, Typeable (liftOT ot')) => [Requirement zone (OT6 a b c d e f)] -> (ZO zone (OT6 a b c d e f) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked6

instance (Inst7 IsObjectType a b c d e f g) => AsWithMaskedObject (OT7 a b c d e f g) where
  masked :: (Inst7 IsObjectType a b c d e f g, Typeable (liftOT ot')) => [Requirement zone (OT7 a b c d e f g)] -> (ZO zone (OT7 a b c d e f g) -> liftOT ot') -> WithMaskedObject liftOT zone ot'
  masked = Masked7

class AsWithMaskedObjects ot where
  maskeds ::
    forall zone liftOT ot'.
    (Typeable (liftOT ot')) =>
    [Requirement zone ot] ->
    (List (ZO zone ot) -> liftOT ot') ->
    WithMaskedObjects liftOT zone ot'

instance (Inst1 IsObjectType a) => AsWithMaskedObjects (OT1 a) where
  maskeds :: (Inst1 IsObjectType a, Typeable (liftOT ot')) => [Requirement zone (OT1 a)] -> (List (ZO zone (OT1 a)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds1

instance (Inst2 IsObjectType a b) => AsWithMaskedObjects (OT2 a b) where
  maskeds :: (Inst2 IsObjectType a b, Typeable (liftOT ot')) => [Requirement zone (OT2 a b)] -> (List (ZO zone (OT2 a b)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds2

instance (Inst3 IsObjectType a b c) => AsWithMaskedObjects (OT3 a b c) where
  maskeds :: (Inst3 IsObjectType a b c, Typeable (liftOT ot')) => [Requirement zone (OT3 a b c)] -> (List (ZO zone (OT3 a b c)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds3

instance (Inst4 IsObjectType a b c d) => AsWithMaskedObjects (OT4 a b c d) where
  maskeds :: (Inst4 IsObjectType a b c d, Typeable (liftOT ot')) => [Requirement zone (OT4 a b c d)] -> (List (ZO zone (OT4 a b c d)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds4

instance (Inst5 IsObjectType a b c d e) => AsWithMaskedObjects (OT5 a b c d e) where
  maskeds :: (Inst5 IsObjectType a b c d e, Typeable (liftOT ot')) => [Requirement zone (OT5 a b c d e)] -> (List (ZO zone (OT5 a b c d e)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds5

instance (Inst6 IsObjectType a b c d e f) => AsWithMaskedObjects (OT6 a b c d e f) where
  maskeds :: (Inst6 IsObjectType a b c d e f, Typeable (liftOT ot')) => [Requirement zone (OT6 a b c d e f)] -> (List (ZO zone (OT6 a b c d e f)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds6

instance (Inst7 IsObjectType a b c d e f g) => AsWithMaskedObjects (OT7 a b c d e f g) where
  maskeds :: (Inst7 IsObjectType a b c d e f g, Typeable (liftOT ot')) => [Requirement zone (OT7 a b c d e f g)] -> (List (ZO zone (OT7 a b c d e f g)) -> liftOT ot') -> WithMaskedObjects liftOT zone ot'
  maskeds = Maskeds7

type family ThisFromOTN zone ot where
  ThisFromOTN zone (OT1 a) = ZO zone (OT1 a)
  ThisFromOTN zone (OT2 a b) = (ZO zone (OT1 a), ZO zone (OT1 b))
  ThisFromOTN zone (OT3 a b c) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c))
  ThisFromOTN zone (OT4 a b c d) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d))
  ThisFromOTN zone (OT5 a b c d e) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d), ZO zone (OT1 e))
  ThisFromOTN zone (OT6 a b c d e f) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d), ZO zone (OT1 e), ZO zone (OT1 f))

type family OT1FromOTN ot where
  OT1FromOTN (OT1 a) = OT1 a
  OT1FromOTN (OT2 a b) = OT1 a
  OT1FromOTN (OT3 a b c) = OT1 a
  OT1FromOTN (OT4 a b c d) = OT1 a
  OT1FromOTN (OT5 a b c d e) = OT1 a
  OT1FromOTN (OT6 a b c d e f) = OT1 a

class (IsZO zone ot) => AsWithThis zone ot where
  thisObject :: (ThisFromOTN zone ot -> liftOT ot) -> WithThis liftOT zone ot
  thisObject1 :: (ZO zone (OT1FromOTN ot) -> liftOT ot) -> WithThis liftOT zone ot

instance (IsZO zone (OT1 a)) => AsWithThis zone (OT1 a) where
  thisObject :: (IsZO zone (OT1 a)) => (ThisFromOTN zone (OT1 a) -> liftOT (OT1 a)) -> WithThis liftOT zone (OT1 a)
  thisObject = case litOTN @(OT1 a) of
    OT1 -> This1

  thisObject1 :: (IsZO zone (OT1 a)) => (ZO zone (OT1FromOTN (OT1 a)) -> liftOT (OT1 a)) -> WithThis liftOT zone (OT1 a)
  thisObject1 = case litOTN @(OT1 a) of
    OT1 -> This1

instance (IsZO zone (OT2 a b)) => AsWithThis zone (OT2 a b) where
  thisObject :: (IsZO zone (OT2 a b)) => (ThisFromOTN zone (OT2 a b) -> liftOT (OT2 a b)) -> WithThis liftOT zone (OT2 a b)
  thisObject = case litOTN @(OT2 a b) of
    OT2 -> This2

  thisObject1 :: (IsZO zone (OT2 a b)) => (ZO zone (OT1FromOTN (OT2 a b)) -> liftOT (OT2 a b)) -> WithThis liftOT zone (OT2 a b)
  thisObject1 = case litOTN @(OT2 a b) of
    OT2 -> \goThis1 -> This2 \(a, _) -> goThis1 a

instance (IsZO zone (OT3 a b c)) => AsWithThis zone (OT3 a b c) where
  thisObject :: (IsZO zone (OT3 a b c)) => (ThisFromOTN zone (OT3 a b c) -> liftOT (OT3 a b c)) -> WithThis liftOT zone (OT3 a b c)
  thisObject = case litOTN @(OT3 a b c) of
    OT3 -> This3

  thisObject1 :: (IsZO zone (OT3 a b c)) => (ZO zone (OT1FromOTN (OT3 a b c)) -> liftOT (OT3 a b c)) -> WithThis liftOT zone (OT3 a b c)
  thisObject1 = case litOTN @(OT3 a b c) of
    OT3 -> \goThis1 -> This3 \(a, _, _) -> goThis1 a

instance (IsZO zone (OT4 a b c d)) => AsWithThis zone (OT4 a b c d) where
  thisObject :: (IsZO zone (OT4 a b c d)) => (ThisFromOTN zone (OT4 a b c d) -> liftOT (OT4 a b c d)) -> WithThis liftOT zone (OT4 a b c d)
  thisObject = case litOTN @(OT4 a b c d) of
    OT4 -> This4

  thisObject1 :: (IsZO zone (OT4 a b c d)) => (ZO zone (OT1FromOTN (OT4 a b c d)) -> liftOT (OT4 a b c d)) -> WithThis liftOT zone (OT4 a b c d)
  thisObject1 = case litOTN @(OT4 a b c d) of
    OT4 -> \goThis1 -> This4 \(a, _, _, _) -> goThis1 a

instance (IsZO zone (OT5 a b c d e)) => AsWithThis zone (OT5 a b c d e) where
  thisObject :: (IsZO zone (OT5 a b c d e)) => (ThisFromOTN zone (OT5 a b c d e) -> liftOT (OT5 a b c d e)) -> WithThis liftOT zone (OT5 a b c d e)
  thisObject = case litOTN @(OT5 a b c d e) of
    OT5 -> This5

  thisObject1 :: (IsZO zone (OT5 a b c d e)) => (ZO zone (OT1FromOTN (OT5 a b c d e)) -> liftOT (OT5 a b c d e)) -> WithThis liftOT zone (OT5 a b c d e)
  thisObject1 = case litOTN @(OT5 a b c d e) of
    OT5 -> \goThis1 -> This5 \(a, _, _, _, _) -> goThis1 a

instance (IsZO zone (OT6 a b c d e f)) => AsWithThis zone (OT6 a b c d e f) where
  thisObject :: (IsZO zone (OT6 a b c d e f)) => (ThisFromOTN zone (OT6 a b c d e f) -> liftOT (OT6 a b c d e f)) -> WithThis liftOT zone (OT6 a b c d e f)
  thisObject = case litOTN @(OT6 a b c d e f) of
    OT6 -> This6

  thisObject1 :: (IsZO zone (OT6 a b c d e f)) => (ZO zone (OT1FromOTN (OT6 a b c d e f)) -> liftOT (OT6 a b c d e f)) -> WithThis liftOT zone (OT6 a b c d e f)
  thisObject1 = case litOTN @(OT6 a b c d e f) of
    OT6 -> \goThis1 -> This6 \(a, _, _, _, _, _) -> goThis1 a

activatedOT' :: (AsWithThis zone ot) => (ThisFromOTN zone ot -> ElectOT 'TargetStage (ActivatedAbility zone) ot) -> WithThisAbility zone ot
activatedOT' = WithThisActivated . thisObject

activated' :: (AsWithThis zone ot) => (ThisFromOTN zone ot -> Elect 'TargetStage (ActivatedAbility zone ot) ot) -> WithThisAbility zone ot
activated' = activatedOT' . (ElectOT .)

activated :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> Elect 'TargetStage (ActivatedAbility zone ot) ot) -> SomeZone WithThisAbility ot
activated = SomeZone . activated'

static' :: (AsWithThis zone ot) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> WithThisAbility zone ot
static' = WithThisStatic . thisObject

static :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> SomeZone WithThisAbility ot
static = SomeZone . static'

-- | Alias for `static` in case someone wants to use `StaticPointers` extension.
static_ :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> SomeZone WithThisAbility ot
static_ = static

triggered' :: (AsWithThis zone ot) => (ThisFromOTN zone ot -> TriggeredAbility zone ot) -> WithThisAbility zone ot
triggered' = WithThisTriggered . thisObject

triggered :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> TriggeredAbility zone ot) -> SomeZone WithThisAbility ot
triggered = SomeZone . triggered'

class AsDamage a where
  asDamage :: a -> Damage 'Var

instance AsDamage Integer where
  asDamage :: Integer -> Damage 'Var
  asDamage n = asDamage (fromInteger n :: Int)

instance AsDamage Int where
  asDamage :: Int -> Damage 'Var
  asDamage = Damage

instance AsDamage (Damage 'Var) where
  asDamage :: Damage 'Var -> Damage 'Var
  asDamage = id

instance AsDamage (Variable Int) where
  asDamage :: Variable Int -> Damage 'Var
  asDamage = VariableDamage

manaCost :: (ToManaCost a) => a -> Cost
manaCost = ManaCost . toManaCost

noCost :: Cost
noCost = OrCosts []

dealDamage ::
  ( AsDamageSource source
  , AsCreaturePlayerPlaneswalker target
  , AsDamage damage
  , IsZO zone OTNDamageSource
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
  (AsAny ot', IsZO zone ot') => ZO zone ot' -> (ZOPlayer -> Elect s e ot) -> Elect s e ot
controllerOf = ControllerOf . asAny

sacrifice ::
  (CoPermanent ot) =>
  ZOPlayer ->
  [Requirement 'ZBattlefield ot] ->
  Effect 'OneShot
sacrifice = Sacrifice

changeTo ::
  (AsPermanent ot, CoPermanent ot) =>
  ZO 'ZBattlefield ot ->
  Card ot ->
  Effect 'Continuous
changeTo = ChangeTo . asPermanent

destroy :: (AsPermanent ot) => ZO 'ZBattlefield ot -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility ::
  (AsActivatedOrTriggeredAbility ot) => ZO 'ZStack ot -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: (AsSpell ot) => ZO 'ZStack ot -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

is :: (IsZone zone, CoAny ot) => ZO zone ot -> Requirement zone ot
is = Is

satisfies ::
  (IsZone zone, CoAny ot) => ZO zone ot -> [Requirement zone ot] -> Condition
satisfies = Satisfies

sacrificeCost :: (CoPermanent ot') => [Requirement 'ZBattlefield ot'] -> Cost
sacrificeCost = SacrificeCost

tapCost :: (CoPermanent ot') => [Requirement 'ZBattlefield ot'] -> Cost
tapCost = TapCost

isTapped :: (CoPermanent ot) => Requirement 'ZBattlefield ot
isTapped = IsTapped

addToBattlefield :: (CoPermanent ot) => ZOPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield

ofColors :: (IsZO zone ot, ColorsLike c) => c -> Requirement zone ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost

instance AsCost Cost ot where
  asCost :: Cost -> Cost
  asCost = id

instance AsCost (ManaCost 'Var) ot where
  asCost :: ManaCost 'Var -> Cost
  asCost = ManaCost

class ElectEffect effect elect where
  effect :: effect -> elect

instance (Typeable ef) => ElectEffect (Effect ef) (Elect 'ResolveStage (Effect ef) ot) where
  effect :: (Typeable ef) => Effect ef -> Elect 'ResolveStage (Effect ef) ot
  effect = Effect . pure

instance (Typeable ef) => ElectEffect [Effect ef] (Elect 'ResolveStage (Effect ef) ot) where
  effect :: (Typeable ef) => [Effect ef] -> Elect 'ResolveStage (Effect ef) ot
  effect = Effect

instance ElectEffect (Effect 'Continuous) (Elect 'ResolveStage (Effect 'OneShot) ot) where
  effect :: Effect 'Continuous -> Elect 'ResolveStage (Effect 'OneShot) ot
  effect = Effect . pure . EffectContinuous

instance (Typeable ef) => ElectEffect (Effect ef) (ElectTargetedEffect (Effect ef) ot) where
  effect :: (Typeable ef) => Effect ef -> ElectTargetedEffect (Effect ef) ot
  effect = EndTargets . effect

instance (Typeable ef) => ElectEffect [Effect ef] (ElectTargetedEffect (Effect ef) ot) where
  effect :: (Typeable ef) => [Effect ef] -> ElectTargetedEffect (Effect ef) ot
  effect = EndTargets . effect

class EventLike s el where
  event :: el -> Elect s el ot

instance EventLike 'ResolveStage Event where
  event :: Event -> Elect 'ResolveStage Event ot
  event = Event

instance EventLike 'IntrinsicStage EventListener where
  event :: EventListener -> Elect 'IntrinsicStage EventListener ot
  event = Listen

class AsIfThen (s :: ElectStage) (el :: Type) (ot :: Type) where
  thenEmpty :: Elect s el ot

class (AsIfThen s el ot) => AsIfElse s el ot where
  elseEmpty :: Else s el ot
  default elseEmpty :: (AsIfThenElse s el ot) => Else s el ot
  elseEmpty = liftElse thenEmpty

class (AsIfThen (s :: ElectStage) (el :: Type) (ot :: Type)) => AsIfThenElse s el ot where
  liftElse :: Elect s el ot -> Else s el ot

instance AsIfThen 'ResolveStage (Effect 'OneShot) ot where
  thenEmpty :: Elect 'ResolveStage (Effect 'OneShot) ot
  thenEmpty = Effect []

instance AsIfElse 'ResolveStage (Effect 'OneShot) ot

instance AsIfThen 'IntrinsicStage EventListener ot where
  thenEmpty :: Elect 'IntrinsicStage EventListener ot
  thenEmpty = event $ Events []

instance AsIfElse 'IntrinsicStage EventListener ot where
  elseEmpty :: Else 'IntrinsicStage EventListener ot
  elseEmpty = ElseEvent

instance AsIfThenElse 'ResolveStage (Effect 'OneShot) ot where
  liftElse :: Elect 'ResolveStage (Effect 'OneShot) ot -> Else 'ResolveStage (Effect 'OneShot) ot
  liftElse = ElseEffect

ifThen :: (AsIfElse s el ot) => Condition -> Elect s el ot -> Elect s el ot
ifThen cond then_ = If cond then_ elseEmpty

ifElse :: (AsIfElse s el ot) => Condition -> Elect s el ot -> Elect s el ot
ifElse cond else_ = If (CNot cond) else_ elseEmpty

ifThenElse ::
  (AsIfThenElse s el ot) => Condition -> Elect s el ot -> Elect s el ot -> Elect s el ot
ifThenElse cond then_ else_ = If cond then_ $ liftElse else_

isBasic :: (IsZone zone) => Requirement zone OTNLand
isBasic = ROr $ map (HasLandType . BasicLand) [minBound ..]

-- XXX: Fixme. Non-basic lands can have basic land types.
nonBasic :: (IsZone zone) => Requirement zone OTNLand
nonBasic = RAnd $ map (Not . HasLandType . BasicLand) [minBound ..]

nonBlack :: (IsZO zone ot) => Requirement zone ot
nonBlack = Not $ ofColors Black

colored :: (IsZO zone ot) => Requirement zone ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: (IsZO zone ot) => Requirement zone ot
colorless = Not colored

type FinColor = Fin Color (ToNat 4)

chooseAnyColor :: (CoNonIntrinsicStage s) => ZOPlayer -> (Variable FinColor -> Elect s el ot) -> Elect s el ot
chooseAnyColor player = ChooseOption player list
 where
  list = LS White CTrue $ LS Blue CTrue $ LS Black CTrue $ LS Red CTrue $ LZ Green CTrue

addManaAnyColor :: Variable FinColor -> ZOPlayer -> Int -> Effect 'OneShot
addManaAnyColor color player amount =
  EffectCase
    CaseFin
      { caseFin = color
      , ofFin = LS () (go W) $ LS () (go U) $ LS () (go B) $ LS () (go R) $ LZ () (go G)
      }
 where
  go :: (ToManaPool 'NonSnow (ManaSymbol mt, Int)) => ManaSymbol mt -> Effect 'OneShot
  go sym = AddMana player $ toManaPool (sym, amount)

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
  (IsZO zone ot) =>
  SomeZone WithThisAbility ot ->
  Requirement zone ot
hasAbility = HasAbility

becomesTapped ::
  (CoPermanent ot) =>
  WithLinkedObject (Elect 'ResolveStage (Effect 'OneShot)) 'ZBattlefield ot ->
  EventListener
becomesTapped = BecomesTapped

untilEndOfTurn :: Effect 'Continuous -> Effect 'OneShot
untilEndOfTurn =
  EffectContinuous . Until (event $ TimePoint (StepBegin CleanupStep) Proxy)

gainAbility :: (CoAny ot) => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
gainAbility = GainAbility

loseAbility :: (CoAny ot) => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
loseAbility = LoseAbility

gainControl :: (CoAny ot) => ZOPlayer -> ZO 'ZBattlefield ot -> Effect 'Continuous
gainControl = GainControl

class HasLandType a where
  hasLandType :: (IsZone zone) => a -> Requirement zone OTNLand

instance HasLandType BasicLandType where
  hasLandType :: (IsZone zone) => BasicLandType -> Requirement zone OTNLand
  hasLandType = HasLandType . BasicLand

instance HasLandType LandType where
  hasLandType :: (IsZone zone) => LandType -> Requirement zone OTNLand
  hasLandType = HasLandType

putOntoBattlefield ::
  (IsZone zone, CoPermanent ot) => ZOPlayer -> ZO zone ot -> Effect 'OneShot
putOntoBattlefield = PutOntoBattlefield

searchLibrary ::
  (CoCard ot) =>
  ZOPlayer ->
  ZOPlayer ->
  WithLinkedObject (Elect 'ResolveStage (Effect 'OneShot)) 'ZLibrary ot ->
  Effect 'OneShot
searchLibrary = SearchLibrary

type CanHaveTrivialManaAbility ot =
  ( CoPermanent (OT1FromOTN ot)
  , CoAny (OT1FromOTN ot)
  , AsAny (OT1FromOTN ot)
  , AsWithThis 'ZBattlefield ot
  )

mkTrivialManaAbility ::
  forall ot ot1 zo.
  (ot1 ~ OT1FromOTN ot) =>
  (zo ~ ZO 'ZBattlefield ot1) =>
  (CanHaveTrivialManaAbility ot) =>
  Maybe BasicLandType ->
  WithThisActivated 'ZBattlefield ot
mkTrivialManaAbility mTy = thisObject1 \this ->
  ElectOT $
    controllerOf this \you ->
      ElectActivated
        Ability
          { activated_cost = tapCost [is this]
          , activated_effect = effect $ AddMana you case mTy of
              Just ty -> case ty of
                Plains -> toManaPool W
                Island -> toManaPool U
                Swamp -> toManaPool B
                Mountain -> toManaPool R
                Forest -> toManaPool G
              Nothing -> toManaPool C
          }

plainsManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
plainsManaAbility = mkTrivialManaAbility $ Just Plains

islandManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
islandManaAbility = mkTrivialManaAbility $ Just Island

swampManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
swampManaAbility = mkTrivialManaAbility $ Just Swamp

mountainManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
mountainManaAbility = mkTrivialManaAbility $ Just Mountain

forestManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
forestManaAbility = mkTrivialManaAbility $ Just Forest

wastesManaAbility :: (CanHaveTrivialManaAbility ot) => WithThisActivated 'ZBattlefield ot
wastesManaAbility = mkTrivialManaAbility Nothing

trivialManaAbility :: (CanHaveTrivialManaAbility ot) => Maybe BasicLandType -> WithThisActivated 'ZBattlefield ot
trivialManaAbility = \case
  Just ty -> case ty of
    Plains -> plainsManaAbility
    Island -> islandManaAbility
    Swamp -> swampManaAbility
    Mountain -> mountainManaAbility
    Forest -> forestManaAbility
  Nothing -> wastesManaAbility

trivialManaAbilities :: (CanHaveTrivialManaAbility ot) => [WithThisActivated 'ZBattlefield ot]
trivialManaAbilities =
  [ plainsManaAbility
  , islandManaAbility
  , swampManaAbility
  , mountainManaAbility
  , forestManaAbility
  , wastesManaAbility
  ]

mkBasicLandwalk :: BasicLandType -> SomeZone WithThisAbility OTNCreature
mkBasicLandwalk ty = static \_this -> Landwalk [HasLandType $ BasicLand ty]

swampwalk :: SomeZone WithThisAbility OTNCreature
swampwalk = mkBasicLandwalk Swamp

paidCost :: a -> NatList () (ToNat 0) a -> NatList () (ToNat 1) a
paidCost = LS ()

didNotPayCost :: a -> NatList () (ToNat 0) a
didNotPayCost = LZ ()

class
  ( ToManaPool 'NonSnow (ManaSymbol mt1)
  , ToManaPool 'NonSnow (ManaSymbol mt2)
  , ToManaCost (ManaSymbol mth)
  ) =>
  ToHybrid (mt1 :: ManaType) (mt2 :: ManaType) (mth :: ManaType)
    | mt1 mt2 -> mth
    , mth -> mt1 mt2
  where
  toHybrid :: ManaSymbol mt1 -> ManaSymbol mt2 -> ManaSymbol mth

instance ToHybrid 'TyW 'TyU 'TyWU where
  toHybrid :: ManaSymbol 'TyW -> ManaSymbol 'TyU -> ManaSymbol 'TyWU
  toHybrid _ _ = WU

instance ToHybrid 'TyU 'TyB 'TyUB where
  toHybrid :: ManaSymbol 'TyU -> ManaSymbol 'TyB -> ManaSymbol 'TyUB
  toHybrid _ _ = UB

instance ToHybrid 'TyB 'TyR 'TyBR where
  toHybrid :: ManaSymbol 'TyB -> ManaSymbol 'TyR -> ManaSymbol 'TyBR
  toHybrid _ _ = BR

instance ToHybrid 'TyR 'TyG 'TyRG where
  toHybrid :: ManaSymbol 'TyR -> ManaSymbol 'TyG -> ManaSymbol 'TyRG
  toHybrid _ _ = RG

instance ToHybrid 'TyG 'TyW 'TyGW where
  toHybrid :: ManaSymbol 'TyG -> ManaSymbol 'TyW -> ManaSymbol 'TyGW
  toHybrid _ _ = GW

instance ToHybrid 'TyW 'TyB 'TyWB where
  toHybrid :: ManaSymbol 'TyW -> ManaSymbol 'TyB -> ManaSymbol 'TyWB
  toHybrid _ _ = WB

instance ToHybrid 'TyU 'TyR 'TyUR where
  toHybrid :: ManaSymbol 'TyU -> ManaSymbol 'TyR -> ManaSymbol 'TyUR
  toHybrid _ _ = UR

instance ToHybrid 'TyB 'TyG 'TyBG where
  toHybrid :: ManaSymbol 'TyB -> ManaSymbol 'TyG -> ManaSymbol 'TyBG
  toHybrid _ _ = BG

instance ToHybrid 'TyR 'TyW 'TyRW where
  toHybrid :: ManaSymbol 'TyR -> ManaSymbol 'TyW -> ManaSymbol 'TyRW
  toHybrid _ _ = RW

instance ToHybrid 'TyG 'TyU 'TyGU where
  toHybrid :: ManaSymbol 'TyG -> ManaSymbol 'TyU -> ManaSymbol 'TyGU
  toHybrid _ _ = GU
