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
  playerPays,
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
  yourCardFacet,
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
  OTN,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNCreature,
  OTNDamageSource,
  OTNLand,
  OTNPlayer,
 )
import safe MtgPure.Model.Object.OTN_ (OTN' (..))
import safe MtgPure.Model.Object.Singleton.Any (CoAny (..))
import safe MtgPure.Model.Object.Singleton.Card (CoCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  CardFacet,
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (..),
  ElectOneShotEffect,
  ElectTargetedEffect,
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  List,
  NonProxy (..),
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
  toCard = id

instance ToCard (Card ot) where
  toCard card = case card of
    Card{} -> AnyCard1 card
    DoubleSidedCard{} -> AnyCard2 card
    SplitCard{} -> AnyCard2 card

class ToToken token where
  toToken :: token -> AnyToken

instance ToToken AnyToken where
  toToken = id

instance CoPermanent ot => ToToken (Token ot) where
  toToken (Token x) = AnyToken $ Token x

class Typeable x => CoNonProxy x where
  coNonProxy :: NonProxy x

instance (Typeable s, Typeable ef) => CoNonProxy (Elect s (Effect ef)) where
  coNonProxy = NonProxyElectEffect

-- instance (Typeable ot) => CoNonProxy (Elect 'Pre (Elect 'Post (Effect 'Continuous) ot)) where
--   coNonProxy = NonProxyElectPrePostEffect

class (IsOTN ot, Typeable liftOT) => AsWithLinkedObject ot zone liftOT where
  linked :: [Requirement zone ot] -> (ZO zone ot -> liftOT ot) -> WithLinkedObject zone liftOT ot

instance (CoNonProxy x, Inst1 IsObjectType a) => AsWithLinkedObject (OT1 a) zone x where
  linked = Linked1 coNonProxy

instance (CoNonProxy x, Inst2 IsObjectType a b) => AsWithLinkedObject (OT2 a b) zone x where
  linked = Linked2 coNonProxy

instance (CoNonProxy x, Inst3 IsObjectType a b c) => AsWithLinkedObject (OT3 a b c) zone x where
  linked = Linked3 coNonProxy

instance (CoNonProxy x, Inst4 IsObjectType a b c d) => AsWithLinkedObject (OT4 a b c d) zone x where
  linked = Linked4 coNonProxy

instance (CoNonProxy x, Inst5 IsObjectType a b c d e) => AsWithLinkedObject (OT5 a b c d e) zone x where
  linked = Linked5 coNonProxy

class AsWithMaskedObject ot where
  masked :: forall zone liftOT ot'. Typeable (liftOT ot') => [Requirement zone ot] -> (ZO zone ot -> liftOT ot') -> WithMaskedObject zone liftOT ot'

instance Inst1 IsObjectType a => AsWithMaskedObject (OT1 a) where
  masked = Masked1

instance Inst2 IsObjectType a b => AsWithMaskedObject (OT2 a b) where
  masked = Masked2

instance Inst3 IsObjectType a b c => AsWithMaskedObject (OT3 a b c) where
  masked = Masked3

instance Inst4 IsObjectType a b c d => AsWithMaskedObject (OT4 a b c d) where
  masked = Masked4

instance Inst5 IsObjectType a b c d e => AsWithMaskedObject (OT5 a b c d e) where
  masked = Masked5

instance Inst6 IsObjectType a b c d e f => AsWithMaskedObject (OT6 a b c d e f) where
  masked = Masked6

class AsWithMaskedObjects ot where
  maskeds :: forall zone liftOT ot'. Typeable (liftOT ot') => [Requirement zone ot] -> (List (ZO zone ot) -> liftOT ot') -> WithMaskedObjects zone liftOT ot'

instance Inst1 IsObjectType a => AsWithMaskedObjects (OT1 a) where
  maskeds = Maskeds1

instance Inst2 IsObjectType a b => AsWithMaskedObjects (OT2 a b) where
  maskeds = Maskeds2

instance Inst3 IsObjectType a b c => AsWithMaskedObjects (OT3 a b c) where
  maskeds = Maskeds3

instance Inst4 IsObjectType a b c d => AsWithMaskedObjects (OT4 a b c d) where
  maskeds = Maskeds4

instance Inst5 IsObjectType a b c d e => AsWithMaskedObjects (OT5 a b c d e) where
  maskeds = Maskeds5

instance Inst6 IsObjectType a b c d e f => AsWithMaskedObjects (OT6 a b c d e f) where
  maskeds = Maskeds6

type family ThisFromOTN zone ot where
  ThisFromOTN zone (OT1 a) = ZO zone (OT1 a)
  ThisFromOTN zone (OT2 a b) = (ZO zone (OT1 a), ZO zone (OT1 b))
  ThisFromOTN zone (OT3 a b c) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c))
  ThisFromOTN zone (OT4 a b c d) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d))
  ThisFromOTN zone (OT5 a b c d e) = (ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d), ZO zone (OT1 e))

type family OT1FromOTN ot where
  OT1FromOTN (OT1 a) = OT1 a
  OT1FromOTN (OT2 a b) = OT1 a
  OT1FromOTN (OT3 a b c) = OT1 a
  OT1FromOTN (OT4 a b c d) = OT1 a
  OT1FromOTN (OT5 a b c d e) = OT1 a

class IsZO zone ot => AsWithThis zone ot where
  thisObject :: (ThisFromOTN zone ot -> liftOT ot) -> WithThis liftOT zone ot
  thisObject1 :: (ZO zone (OT1FromOTN ot) -> liftOT ot) -> WithThis liftOT zone ot

instance IsZO zone (OT1 a) => AsWithThis zone (OT1 a) where
  thisObject = case litOTN @(OT1 a) of
    OT1 -> This1
  thisObject1 = case litOTN @(OT1 a) of
    OT1 -> This1

instance IsZO zone (OT2 a b) => AsWithThis zone (OT2 a b) where
  thisObject = case litOTN @(OT2 a b) of
    OT2 -> This2
  thisObject1 = case litOTN @(OT2 a b) of
    OT2 -> \goThis1 -> This2 \(a, _) -> goThis1 a

instance IsZO zone (OT3 a b c) => AsWithThis zone (OT3 a b c) where
  thisObject = case litOTN @(OT3 a b c) of
    OT3 -> This3
  thisObject1 = case litOTN @(OT3 a b c) of
    OT3 -> \goThis1 -> This3 \(a, _, _) -> goThis1 a

instance IsZO zone (OT4 a b c d) => AsWithThis zone (OT4 a b c d) where
  thisObject = case litOTN @(OT4 a b c d) of
    OT4 -> This4
  thisObject1 = case litOTN @(OT4 a b c d) of
    OT4 -> \goThis1 -> This4 \(a, _, _, _) -> goThis1 a

instance IsZO zone (OT5 a b c d e) => AsWithThis zone (OT5 a b c d e) where
  thisObject = case litOTN @(OT5 a b c d e) of
    OT5 -> This5
  thisObject1 = case litOTN @(OT5 a b c d e) of
    OT5 -> \goThis1 -> This5 \(a, _, _, _, _) -> goThis1 a

activatedOT' :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> ElectOT 'TargetStage (ActivatedAbility zone) ot) -> WithThisAbility zone ot
activatedOT' = WithThisActivated . thisObject

activated' :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> Elect 'TargetStage (ActivatedAbility zone ot) ot) -> WithThisAbility zone ot
activated' = activatedOT' . (ElectOT .)

activated :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> Elect 'TargetStage (ActivatedAbility zone ot) ot) -> SomeZone WithThisAbility ot
activated = SomeZone . activated'

static' :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> WithThisAbility zone ot
static' = WithThisStatic . thisObject

static :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> SomeZone WithThisAbility ot
static = SomeZone . static'

-- | Alias for `static` in case someone wants to use `StaticPointers` extension.
static_ :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> StaticAbility zone ot) -> SomeZone WithThisAbility ot
static_ = static

triggered' :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> TriggeredAbility zone ot) -> WithThisAbility zone ot
triggered' = WithThisTriggered . thisObject

triggered :: (AsWithThis zone ot, ot ~ OTN x) => (ThisFromOTN zone ot -> TriggeredAbility zone ot) -> SomeZone WithThisAbility ot
triggered = SomeZone . triggered'

yourCardFacet :: (ZOPlayer -> CardFacet ot) -> Elect 'IntrinsicStage (CardFacet ot) ot
yourCardFacet = Your . (ElectCardFacet .)

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

manaCost :: ToManaCost a => a -> Cost ot
manaCost = ManaCost . toManaCost

noCost :: Cost ot
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
  CoPermanent ot =>
  ZOPlayer ->
  [Requirement 'ZBattlefield ot] ->
  Effect 'OneShot
sacrifice = Sacrifice

changeTo ::
  (AsPermanent ot, CoPermanent ot, ot ~ OTN x) =>
  ZO 'ZBattlefield ot ->
  Card ot ->
  Effect 'Continuous
changeTo = ChangeTo . asPermanent

destroy :: AsPermanent ot => ZO 'ZBattlefield ot -> Effect 'OneShot
destroy = Destroy . asPermanent

counterAbility ::
  AsActivatedOrTriggeredAbility ot => ZO 'ZStack ot -> Effect 'OneShot
counterAbility = CounterAbility . asActivatedOrTriggeredAbility

counterSpell :: AsSpell ot => ZO 'ZStack ot -> Effect 'OneShot
counterSpell = CounterSpell . asSpell

is :: (IsZone zone, CoAny ot) => ZO zone ot -> Requirement zone ot
is = Is

satisfies ::
  (IsZone zone, CoAny ot) => ZO zone ot -> [Requirement zone ot] -> Condition
satisfies = Satisfies

sacrificeCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
sacrificeCost = SacrificeCost

tapCost :: CoPermanent ot' => [Requirement 'ZBattlefield ot'] -> Cost ot
tapCost = TapCost

isTapped :: CoPermanent ot => Requirement 'ZBattlefield ot
isTapped = IsTapped

addToBattlefield :: CoPermanent ot => ZOPlayer -> Token ot -> Effect 'OneShot
addToBattlefield = AddToBattlefield

ofColors :: (IsZO zone ot, ColorsLike c) => c -> Requirement zone ot
ofColors = OfColors . toColors

class AsCost c ot where
  asCost :: c -> Cost ot

instance AsCost (Cost ot) ot where
  asCost = id

instance AsCost (ManaCost 'Var) ot where
  asCost = ManaCost

playerPays :: (IsZone zone, AsCost c OTNPlayer) => c -> Requirement zone OTNPlayer
playerPays = PlayerPays . asCost

class ElectEffect effect elect where
  effect :: effect -> elect

instance Typeable ef => ElectEffect (Effect ef) (Elect 'ResolveStage (Effect ef) ot) where
  effect = Effect . pure

instance Typeable ef => ElectEffect [Effect ef] (Elect 'ResolveStage (Effect ef) ot) where
  effect = Effect

instance ElectEffect (Effect 'Continuous) (Elect 'ResolveStage (Effect 'OneShot) ot) where
  effect = Effect . pure . EffectContinuous

instance Typeable ef => ElectEffect (Effect ef) (ElectTargetedEffect (Effect ef) ot) where
  effect = EndTargets . effect

instance Typeable ef => ElectEffect [Effect ef] (ElectTargetedEffect (Effect ef) ot) where
  effect = EndTargets . effect

instance ElectEffect (Effect 'Continuous) (ElectOneShotEffect ot) where
  effect = EndTargets . effect

class EventLike el where
  event :: el -> Elect 'ResolveStage el ot

instance EventLike Event where
  event = Event

instance EventLike EventListener where
  event = Listen

class AsIfThen (el :: Type) (ot :: Type) where
  thenEmpty :: Elect 'ResolveStage el ot

class AsIfThen el ot => AsIfElse el ot where
  elseEmpty :: Else el ot
  default elseEmpty :: AsIfThenElse el ot => Else el ot
  elseEmpty = liftElse thenEmpty

class AsIfThen (el :: Type) (ot :: Type) => AsIfThenElse el ot where
  liftElse :: Elect 'ResolveStage el ot -> Else el ot

instance AsIfThen (Effect 'OneShot) ot where
  thenEmpty = Effect []

instance AsIfElse (Effect 'OneShot) ot

instance AsIfThen EventListener ot where
  thenEmpty = event $ Events []

instance AsIfElse EventListener ot where
  elseEmpty = ElseEvent

instance AsIfThenElse (Effect 'OneShot) ot where
  liftElse = ElseEffect

ifThen :: AsIfElse el ot => Condition -> Elect 'ResolveStage el ot -> Elect 'ResolveStage el ot
ifThen cond then_ = If cond then_ elseEmpty

ifElse :: AsIfElse el ot => Condition -> Elect 'ResolveStage el ot -> Elect 'ResolveStage el ot
ifElse cond else_ = If (CNot cond) else_ elseEmpty

ifThenElse ::
  AsIfThenElse el ot => Condition -> Elect 'ResolveStage el ot -> Elect 'ResolveStage el ot -> Elect 'ResolveStage el ot
ifThenElse cond then_ else_ = If cond then_ $ liftElse else_

isBasic :: IsZone zone => Requirement zone OTNLand
isBasic = ROr $ map (HasLandType . BasicLand) [minBound ..]

nonBasic :: IsZone zone => Requirement zone OTNLand
nonBasic = RAnd $ map (Not . HasLandType . BasicLand) [minBound ..]

nonBlack :: IsZO zone ot => Requirement zone ot
nonBlack = Not $ ofColors Black

colored :: IsZO zone ot => Requirement zone ot
colored = ROr $ map ofColors [minBound :: Color ..]

colorless :: IsZO zone ot => Requirement zone ot
colorless = Not colored

type FinColor = Fin Color (ToNat 4)

chooseAnyColor :: CoNonIntrinsicStage s => ZOPlayer -> (Variable FinColor -> Elect s el ot) -> Elect s el ot
chooseAnyColor player = ChooseOption player list
 where
  list = LS CTrue $ LS CTrue $ LS CTrue $ LS CTrue $ LZ CTrue

addManaAnyColor :: Variable FinColor -> ZOPlayer -> Int -> Effect 'OneShot
addManaAnyColor color player amount =
  EffectCase
    CaseFin
      { caseFin = color
      , ofFin = LS (go W) $ LS (go U) $ LS (go B) $ LS (go R) $ LZ (go G)
      }
 where
  go :: ToManaPool 'NonSnow (ManaSymbol mt, Int) => ManaSymbol mt -> Effect 'OneShot
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
  IsZO zone ot =>
  SomeZone WithThisAbility ot ->
  Requirement zone ot
hasAbility = HasAbility

becomesTapped ::
  CoPermanent ot =>
  WithLinkedObject 'ZBattlefield (Elect 'ResolveStage (Effect 'OneShot)) ot ->
  EventListener
becomesTapped = BecomesTapped

untilEndOfTurn :: Effect 'Continuous -> Effect 'OneShot
untilEndOfTurn =
  EffectContinuous . Until (event $ TimePoint (StepBegin CleanupStep) Proxy)

gainAbility :: CoAny ot => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
gainAbility = GainAbility

loseAbility :: CoAny ot => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
loseAbility = LoseAbility

gainControl :: CoAny ot => ZOPlayer -> ZO 'ZBattlefield ot -> Effect 'Continuous
gainControl = GainControl

class HasLandType a where
  hasLandType :: IsZone zone => a -> Requirement zone OTNLand

instance HasLandType BasicLandType where
  hasLandType = HasLandType . BasicLand

instance HasLandType LandType where
  hasLandType = HasLandType

putOntoBattlefield ::
  (IsZone zone, CoPermanent ot) => ZOPlayer -> ZO zone ot -> Effect 'OneShot
putOntoBattlefield = PutOntoBattlefield

searchLibrary ::
  CoCard ot =>
  ZOPlayer ->
  ZOPlayer ->
  WithLinkedObject 'ZLibrary (Elect 'ResolveStage (Effect 'OneShot)) ot ->
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
  ot1 ~ OT1FromOTN ot =>
  zo ~ ZO 'ZBattlefield ot1 =>
  CanHaveTrivialManaAbility ot =>
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

plainsManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
plainsManaAbility = mkTrivialManaAbility $ Just Plains

islandManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
islandManaAbility = mkTrivialManaAbility $ Just Island

swampManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
swampManaAbility = mkTrivialManaAbility $ Just Swamp

mountainManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
mountainManaAbility = mkTrivialManaAbility $ Just Mountain

forestManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
forestManaAbility = mkTrivialManaAbility $ Just Forest

wastesManaAbility :: CanHaveTrivialManaAbility ot => WithThisActivated 'ZBattlefield ot
wastesManaAbility = mkTrivialManaAbility Nothing

trivialManaAbility :: CanHaveTrivialManaAbility ot => Maybe BasicLandType -> WithThisActivated 'ZBattlefield ot
trivialManaAbility = \case
  Just ty -> case ty of
    Plains -> plainsManaAbility
    Island -> islandManaAbility
    Swamp -> swampManaAbility
    Mountain -> mountainManaAbility
    Forest -> forestManaAbility
  Nothing -> wastesManaAbility

trivialManaAbilities :: CanHaveTrivialManaAbility ot => [WithThisActivated 'ZBattlefield ot]
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
  toHybrid _ _ = WU

instance ToHybrid 'TyU 'TyB 'TyUB where
  toHybrid _ _ = UB

instance ToHybrid 'TyB 'TyR 'TyBR where
  toHybrid _ _ = BR

instance ToHybrid 'TyR 'TyG 'TyRG where
  toHybrid _ _ = RG

instance ToHybrid 'TyG 'TyW 'TyGW where
  toHybrid _ _ = GW

instance ToHybrid 'TyW 'TyB 'TyWB where
  toHybrid _ _ = WB

instance ToHybrid 'TyU 'TyR 'TyUR where
  toHybrid _ _ = UR

instance ToHybrid 'TyB 'TyG 'TyBG where
  toHybrid _ _ = BG

instance ToHybrid 'TyR 'TyW 'TyRW where
  toHybrid _ _ = RW

instance ToHybrid 'TyG 'TyU 'TyGU where
  toHybrid _ _ = GU
