{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use camelCase" #-}

module MtgPure.ModelCombinators (
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
  basicManaAbility,
  becomesTapped,
  changeTo,
  chooseAnyColor,
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
  gainAbility,
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
  spellCost,
  tapCost,
  ToCard (..),
  ToToken (..),
  tyAp,
  unlifted_isBasicManaAbility,
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
import safe Data.Nat (Fin, NatList (..), ToNat)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorsLike (ColorsLike (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.LandType (LandType (BasicLand))
import safe MtgPure.Model.Mana (Snow (..))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
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
  OTDamageSource,
  OTLand,
  OTPlayer,
 )
import safe MtgPure.Model.Object.Singleton.Any (CoAny (..))
import safe MtgPure.Model.Object.Singleton.Card (CoCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
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
  List,
  NonProxy (..),
  Requirement (..),
  Token (..),
  WithLinkedObject (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisActivated,
  pattern CTrue,
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.ToManaPool (ToManaPool (..))
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
  IsOT,
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
  maskeds :: forall zone z. Typeable z => [Requirement zone ot] -> (List (ZO zone ot) -> z) -> WithMaskedObjects zone z

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

class IsZO zone ot => AsWithThis ot zone liftOT ots | ot zone liftOT -> ots, ots -> ot zone where
  thisObject :: (ots -> liftOT ot) -> WithThis zone liftOT ot

instance (IsZO zone (OT1 a), Inst1 IsObjectType a) => AsWithThis (OT1 a) zone liftOT (ZO zone (OT1 a)) where
  thisObject = This1

instance (IsZO zone (OT2 a b), Inst2 IsObjectType a b) => AsWithThis (OT2 a b) zone liftOT (ZO zone (OT1 a), ZO zone (OT1 b)) where
  thisObject = This2

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
  (AsPermanent ot, CoPermanent ot, ot ~ OTN x) =>
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

type FinColor = Fin Color (ToNat 4)

chooseAnyColor :: ZOPlayer -> (Variable FinColor -> Elect p el ot) -> Elect p el ot
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

gainAbility :: CoAny ot => ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
gainAbility = GainAbility coAny

loseAbility :: CoAny ot => ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
loseAbility = LoseAbility coAny

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

-- TODO: Synthesize basic land mana abilities inside the engine instead of explicitly making here.
-- Console can use syntax of
--  ActivateAbility LandID w -- tap for white
--  ActivateAbility LandID u -- tap for blue
--  ActivateAbility LandID b -- tap for black
--  ActivateAbility LandID r -- tap for red
--  ActivateAbility LandID g -- tap for green
--  ActivateAbility LandID i -- infer (only avail when has exactly one basic land type)
basicManaAbility :: BasicLandType -> WithThisActivated 'ZBattlefield OTLand
basicManaAbility ty = thisObject \this ->
  controllerOf this \you ->
    ElectActivated $
      Ability
        { activated_cost = tapCost [is this]
        , activated_effect = effect $ AddMana you case ty of
            Plains -> toManaPool W
            Island -> toManaPool U
            Swamp -> toManaPool B
            Mountain -> toManaPool R
            Forest -> toManaPool G
        }

unlifted_isBasicManaAbility :: IsZO zone ot => WithThisActivated zone ot -> Bool
unlifted_isBasicManaAbility ability = case cast ability of
  Just landAbility ->
    let predicate ty = landAbility == basicManaAbility ty
     in any predicate [minBound ..]
  Nothing -> False
