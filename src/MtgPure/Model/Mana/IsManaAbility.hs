{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Mana.IsManaAbility (
  IsManaAbility,
  isManaAbility,
  isTrivialManaAbility,
) where

import safe Data.Kind (Type)
import safe Data.Nat (Fin (..), IsNat, NatList (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Mana.CountMana (CountMana (countMana))
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaPool (ManaPool (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.PromoteIdToObjectN (PromoteIdToObjectN (..))
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  Case (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (..),
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  List (..),
  Requirement (..),
  SomeZone (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis,
  WithThisActivated,
  WithThisZ (..),
 )
import safe MtgPure.Model.Variable (Variable (ReifiedVariable), VariableId' (..))
import safe MtgPure.Model.Zone (IsZone (..))
import safe MtgPure.Model.ZoneObject.Convert (reifyWithThis)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZOPlayer, ZoneObject (ZO))

-- For example cards with abilities that generate mana but are not mana abilities, see:
-- https://boardgames.stackexchange.com/questions/44040/does-mtg-have-a-card-with-a-mana-generating-ability-that-has-a-target-and-is-thu

class (IsManaAbilityImpl a) => IsManaAbility a where
  startingStageTrivial :: StageTrivial

instance IsManaAbility (Ability zone ot) where
  startingStageTrivial :: StageTrivial
  startingStageTrivial = StageAbility

instance IsManaAbility (ActivatedAbility zone ot) where
  startingStageTrivial :: StageTrivial
  startingStageTrivial = StageAbility

instance IsManaAbility (TriggeredAbility zone ot) where
  startingStageTrivial :: StageTrivial
  startingStageTrivial = StageAbility

instance IsManaAbility (SomeZone Ability ot) where
  startingStageTrivial :: StageTrivial
  startingStageTrivial = StageAbility

instance (IsOTN ot) => IsManaAbility (SomeZone (WithThisZ Ability) ot) where
  startingStageTrivial :: (IsOTN ot) => StageTrivial
  startingStageTrivial = StageWithThisAbility

instance (IsZO zone ot) => IsManaAbility (WithThis (Ability zone) zone ot) where
  startingStageTrivial :: (IsZO zone ot) => StageTrivial
  startingStageTrivial = StageWithThisAbility

instance (IsZO zone ot) => IsManaAbility (WithThisActivated zone ot) where
  startingStageTrivial :: (IsZO zone ot) => StageTrivial
  startingStageTrivial = StageWithThisAbility

isManaAbility :: (IsManaAbility a) => a -> Bool
isManaAbility x = case isManaAbilityImpl x of
  IsManaAbility -> True
  _ -> False

type TrivialManaAbilityResult = Maybe (Maybe BasicLandType)

-- | A trivial mana ability is defined to be of the exact form
-- "{T}: Add {#} to your mana pool.", where (#) is a single non-variable mana.
isTrivialManaAbility :: forall a. (IsManaAbility a) => a -> Maybe (Maybe BasicLandType)
isTrivialManaAbility = isTrivialManaAbilityImpl (startingStageTrivial @a)

data Result :: Type where
  Indeterminate :: Result
  IsManaAbility :: Result
  IsNotManaAbility :: Result

instance Semigroup Result where
  (<>) :: Result -> Result -> Result
  (<>) x y = case (x, y) of
    (IsNotManaAbility, _) -> IsNotManaAbility
    (_, IsNotManaAbility) -> IsNotManaAbility
    (Indeterminate, _) -> y
    (_, Indeterminate) -> x
    (IsManaAbility, IsManaAbility) -> IsManaAbility

instance Monoid Result where
  mempty :: Result
  mempty = Indeterminate

data StageTrivial
  = StageWithThisAbility
  | StageAbility
  | StageControllerOf
  | StageElectActivated
  | StageFinal

class IsManaAbilityImpl a where
  isManaAbilityImpl :: a -> Result
  isTrivialManaAbilityImpl :: StageTrivial -> a -> TrivialManaAbilityResult

proxyThisId :: ObjectId
proxyThisId = ObjectId (-1)

proxyYouId :: ObjectId
proxyYouId = ObjectId (-2)

proxyYou :: ZOPlayer
proxyYou = ZO singZone oYou
 where
  oYou = promoteIdToObjectN proxyYouId

dummyObjectId :: ObjectId
dummyObjectId = ObjectId 0

dummyZO :: forall zone ot. (IsZO zone ot) => ZO zone ot
dummyZO = ZO (singZone @zone) oThis
 where
  oThis = promoteIdToObjectN @ot dummyObjectId

mkDummyVar :: a -> Variable a
mkDummyVar = ReifiedVariable (VariableId 0)

class DummyVar a where
  dummyVar :: Variable a

instance DummyVar Int where
  dummyVar :: Variable Int
  dummyVar = mkDummyVar 0

instance DummyVar Power where
  dummyVar :: Variable Power
  dummyVar = mkDummyVar $ Power 0

instance (Typeable user, IsNat n) => DummyVar (Fin user n) where
  dummyVar :: (Typeable user, IsNat n) => Variable (Fin user n)
  dummyVar = mkDummyVar FZ

instance IsManaAbilityImpl (Ability zone ot) where
  isManaAbilityImpl :: Ability zone ot -> Result
  isManaAbilityImpl = \case
    Static{} -> IsNotManaAbility
    Activated withThis -> isManaAbilityImpl withThis
    Triggered withThis -> isManaAbilityImpl withThis

  isTrivialManaAbilityImpl :: StageTrivial -> Ability zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageAbility, Activated electActivated) -> isTrivialManaAbilityImpl StageElectActivated electActivated
    _ -> Nothing

instance IsManaAbilityImpl (ActivatedAbility zone ot) where
  isManaAbilityImpl :: ActivatedAbility zone ot -> Result
  isManaAbilityImpl = \case
    Ability _cost elect -> isManaAbilityImpl elect
    Cycling{} -> IsNotManaAbility

  isTrivialManaAbilityImpl :: StageTrivial -> ActivatedAbility zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageFinal, Ability cost ability) -> case isTapThis cost of
      True -> isTrivialManaAbilityImpl StageFinal ability
      False -> Nothing
    _ -> Nothing

isTapThis :: Cost -> Bool
isTapThis = \case
  TapCost [Is this] -> getObjectId this == proxyThisId
  _ -> False

instance (IsManaAbilityImpl x) => IsManaAbilityImpl (Case x) where
  isManaAbilityImpl :: (IsManaAbilityImpl x) => Case x -> Result
  isManaAbilityImpl = \case
    CaseFin _var list -> isManaAbilityImpl list

  isTrivialManaAbilityImpl :: (IsManaAbilityImpl x) => StageTrivial -> Case x -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (Effect ef) where
  isManaAbilityImpl :: Effect ef -> Result
  isManaAbilityImpl = \case
    AddMana{} -> IsManaAbility
    AddToBattlefield{} -> Indeterminate
    CantBeRegenerated{} -> IsNotManaAbility
    ChangeTo{} -> IsNotManaAbility
    CounterAbility{} -> Indeterminate
    CounterSpell{} -> Indeterminate
    DealDamage{} -> Indeterminate
    Destroy{} -> Indeterminate
    DrawCards{} -> Indeterminate
    EffectCase case_ -> isManaAbilityImpl case_
    EffectContinuous{} -> Indeterminate
    EndTheTurn{} -> Indeterminate
    Exile{} -> Indeterminate
    GainAbility{} -> IsNotManaAbility
    GainControl{} -> Indeterminate
    GainLife{} -> Indeterminate
    LoseAbility{} -> IsNotManaAbility
    LoseLife{} -> Indeterminate
    PutOntoBattlefield{} -> Indeterminate
    Sacrifice{} -> Indeterminate
    SearchLibrary{} -> Indeterminate
    Sequence effects -> mconcat $ map isManaAbilityImpl effects
    ShuffleLibrary{} -> Indeterminate
    StatDelta{} -> IsNotManaAbility
    Tap{} -> Indeterminate
    Untap{} -> Indeterminate
    Until{} -> IsNotManaAbility
    WithList withList -> isManaAbilityImpl withList

  isTrivialManaAbilityImpl :: StageTrivial -> Effect ef -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageFinal, AddMana you pool) -> case getObjectId you == proxyYouId of
      True -> extractSingleManaType pool
      False -> Nothing
    _ -> Nothing

instance IsManaAbilityImpl (Elect s el ot) where
  isManaAbilityImpl :: Elect s el ot -> Result
  isManaAbilityImpl = \case
    ActivePlayer cont -> isManaAbilityImpl $ cont dummyZO
    All wmo -> isManaAbilityImpl wmo
    Choose _zo wmo -> isManaAbilityImpl wmo
    ChooseOption _zo _conds cont -> isManaAbilityImpl $ cont dummyVar
    Condition{} -> IsNotManaAbility
    ControllerOf _zo cont -> isManaAbilityImpl $ cont dummyZO
    Cost{} -> IsNotManaAbility
    Effect effects -> mconcat $ map isManaAbilityImpl effects
    ElectActivated ability -> isManaAbilityImpl ability
    ElectCardCharacteristic{} -> IsNotManaAbility
    ElectCardSpec{} -> IsNotManaAbility
    ElectCase case_ -> isManaAbilityImpl case_
    EndTargets elect -> isManaAbilityImpl elect
    Event{} -> IsNotManaAbility
    If _cond then_ else_ -> isManaAbilityImpl then_ <> isManaAbilityImpl else_
    Listen listener -> isManaAbilityImpl listener
    OwnerOf _zo cont -> isManaAbilityImpl $ cont dummyZO
    PlayerPays _zo _cost cont -> isManaAbilityImpl $ cont dummyVar
    Random wmo -> isManaAbilityImpl wmo
    Target{} -> IsNotManaAbility
    VariableFromPower _zo cont -> isManaAbilityImpl $ cont dummyVar
    VariableInt cont -> isManaAbilityImpl $ cont dummyVar
    Your cont -> isManaAbilityImpl $ cont dummyZO

  isTrivialManaAbilityImpl :: StageTrivial -> Elect s el ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageControllerOf, ControllerOf this cont) -> case getObjectId this == proxyThisId of
      True -> isTrivialManaAbilityImpl StageElectActivated $ cont proxyYou
      False -> Nothing
    (StageElectActivated, ElectActivated ability) -> isTrivialManaAbilityImpl StageFinal ability
    (StageFinal, Effect [e]) -> isTrivialManaAbilityImpl StageFinal e
    _ -> Nothing

instance IsManaAbilityImpl (ElectOT p liftOT ot) where
  isManaAbilityImpl :: ElectOT p liftOT ot -> Result
  isManaAbilityImpl = isManaAbilityImpl . unElectOT

  isTrivialManaAbilityImpl :: StageTrivial -> ElectOT p liftOT ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage = isTrivialManaAbilityImpl stage . unElectOT

extractSingleManaType :: ManaPool 'NonSnow -> TrivialManaAbilityResult
extractSingleManaType pool = case countMana pool of
  1 -> Just case (w, u, b, r, g, c) of
    (1, 0, 0, 0, 0, 0) -> Just Plains
    (0, 1, 0, 0, 0, 0) -> Just Island
    (0, 0, 1, 0, 0, 0) -> Just Swamp
    (0, 0, 0, 1, 0, 0) -> Just Mountain
    (0, 0, 0, 0, 1, 0) -> Just Forest
    (0, 0, 0, 0, 0, 1) -> Nothing
    _ -> error "extractSingleManaType: impossible case (countMana pool == 1)"
  _ -> Nothing
 where
  ManaPool
    { poolW = Mana w
    , poolU = Mana u
    , poolB = Mana b
    , poolR = Mana r
    , poolG = Mana g
    , poolC = Mana c
    } = pool

instance IsManaAbilityImpl (Else s el ot) where
  isManaAbilityImpl :: Else s el ot -> Result
  isManaAbilityImpl = \case
    ElseCost{} -> IsNotManaAbility
    ElseEffect elect -> isManaAbilityImpl elect
    ElseEvent -> Indeterminate

  isTrivialManaAbilityImpl :: StageTrivial -> Else s el ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl Event where
  isManaAbilityImpl :: Event -> Result
  isManaAbilityImpl _ = IsNotManaAbility

  isTrivialManaAbilityImpl :: StageTrivial -> Event -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl EventListener where
  isManaAbilityImpl :: EventListener -> Result
  isManaAbilityImpl = \case
    BecomesTapped wlo -> isManaAbilityImpl wlo
    EntersBattlefield wlo -> isManaAbilityImpl wlo
    EntersNonBattlefield wlo -> isManaAbilityImpl wlo
    Events listeners -> mconcat $ map isManaAbilityImpl listeners
    SpellIsCast wlo -> isManaAbilityImpl wlo
    TimePoint _timePoint elect -> isManaAbilityImpl elect

  isTrivialManaAbilityImpl :: StageTrivial -> EventListener -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance (IsManaAbilityImpl x) => IsManaAbilityImpl (NatList user n x) where
  isManaAbilityImpl :: (IsManaAbilityImpl x) => NatList user n x -> Result
  isManaAbilityImpl = \case
    LZ _ x -> isManaAbilityImpl x
    LS _ x xs -> isManaAbilityImpl x <> isManaAbilityImpl xs

  isTrivialManaAbilityImpl :: (IsManaAbilityImpl x) => StageTrivial -> NatList user n x -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (SomeZone Ability ot) where
  isManaAbilityImpl :: SomeZone Ability ot -> Result
  isManaAbilityImpl = \case
    SomeZone ability -> isManaAbilityImpl ability
    SomeZone2{} -> IsNotManaAbility

  isTrivialManaAbilityImpl :: StageTrivial -> SomeZone Ability ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl stage szAbility = case szAbility of
    SomeZone ability -> isTrivialManaAbilityImpl stage ability
    SomeZone2{} -> Nothing

instance (IsOTN ot) => IsManaAbilityImpl (SomeZone (WithThisZ Ability) ot) where
  isManaAbilityImpl :: (IsOTN ot) => SomeZone (WithThisZ Ability) ot -> Result
  isManaAbilityImpl = \case
    SomeZone (WithThisZ ability) -> isManaAbilityImpl ability

  isTrivialManaAbilityImpl :: (IsOTN ot) => StageTrivial -> SomeZone (WithThisZ Ability) ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl = \case
    StageWithThisAbility -> \case
      SomeZone (WithThisZ ability) -> isTrivialManaAbilityImpl StageWithThisAbility ability
    _ -> const Nothing

instance IsManaAbilityImpl (TriggeredAbility zone ot) where
  isManaAbilityImpl :: TriggeredAbility zone ot -> Result
  isManaAbilityImpl = \case
    When elect -> isManaAbilityImpl elect

  isTrivialManaAbilityImpl :: StageTrivial -> TriggeredAbility zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance (IsZone zone) => IsManaAbilityImpl (WithLinkedObject (Elect s el) zone ot) where
  isManaAbilityImpl :: (IsZone zone) => WithLinkedObject (Elect s el) zone ot -> Result
  isManaAbilityImpl = \case
    Linked1 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked2 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked3 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked4 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked5 _reqs cont -> isManaAbilityImpl $ cont dummyZO

  isTrivialManaAbilityImpl :: (IsZone zone) => StageTrivial -> WithLinkedObject (Elect s el) zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance (IsZone zone) => IsManaAbilityImpl (WithMaskedObject (Elect s el) zone ot) where
  isManaAbilityImpl :: (IsZone zone) => WithMaskedObject (Elect s el) zone ot -> Result
  isManaAbilityImpl = \case
    Masked1 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked2 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked3 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked4 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked5 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked6 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked7 _reqs cont -> isManaAbilityImpl $ cont dummyZO

  isTrivialManaAbilityImpl :: (IsZone zone) => StageTrivial -> WithMaskedObject (Elect s el) zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (WithMaskedObjects (Elect s el) zone ot) where
  isManaAbilityImpl :: WithMaskedObjects (Elect s el) zone ot -> Result
  isManaAbilityImpl = \case
    Maskeds1 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds2 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds3 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds4 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds5 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds6 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds7 _reqs cont -> isManaAbilityImpl $ cont $ List []

  isTrivialManaAbilityImpl :: StageTrivial -> WithMaskedObjects (Elect s el) zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance (IsManaAbilityImpl ret) => IsManaAbilityImpl (WithList ret zone ot) where
  isManaAbilityImpl :: (IsManaAbilityImpl ret) => WithList ret zone ot -> Result
  isManaAbilityImpl = \case
    CountOf _list cont -> isManaAbilityImpl $ cont dummyVar
    Each _list cont -> isManaAbilityImpl $ cont dummyZO
    SuchThat _reqs withList -> isManaAbilityImpl withList

  isTrivialManaAbilityImpl :: (IsManaAbilityImpl ret) => StageTrivial -> WithList ret zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl _ _ = Nothing

instance (IsZO zone ot) => IsManaAbilityImpl (WithThis (Ability zone) zone ot) where
  isManaAbilityImpl :: (IsZO zone ot) => WithThis (Ability zone) zone ot -> Result
  isManaAbilityImpl = isManaAbilityImpl . reifyWithThis dummyObjectId

  isTrivialManaAbilityImpl :: (IsZO zone ot) => StageTrivial -> WithThis (Ability zone) zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl = \case
    StageWithThisAbility ->
      isTrivialManaAbilityImpl StageAbility . reifyWithThis proxyThisId
    _ -> const Nothing

instance (IsZO zone ot) => IsManaAbilityImpl (WithThisActivated zone ot) where
  isManaAbilityImpl :: (IsZO zone ot) => WithThisActivated zone ot -> Result
  isManaAbilityImpl = isManaAbilityImpl . reifyWithThis dummyObjectId

  isTrivialManaAbilityImpl :: (IsZO zone ot) => StageTrivial -> WithThisActivated zone ot -> TrivialManaAbilityResult
  isTrivialManaAbilityImpl = \case
    StageWithThisAbility ->
      isTrivialManaAbilityImpl StageControllerOf . reifyWithThis proxyThisId
    _ -> const Nothing
