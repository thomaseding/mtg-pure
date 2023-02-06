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
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  List (..),
  Requirement (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisActivated,
  WithThisTriggered,
 )
import safe MtgPure.Model.Variable (Variable (ReifiedVariable), VariableId' (..))
import safe MtgPure.Model.Zone (IsZone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZOPlayer, ZoneObject (ZO))

-- For example cards with abilities that generate mana but are not mana abilities, see:
-- https://boardgames.stackexchange.com/questions/44040/does-mtg-have-a-card-with-a-mana-generating-ability-that-has-a-target-and-is-thu

class IsManaAbilityImpl a => IsManaAbility a where
  startingStageTrivial :: StageTrivial

instance IsManaAbility (Ability ot) where
  startingStageTrivial = StageInitial

instance IsManaAbility (ActivatedAbility zone ot) where
  startingStageTrivial = StageInitial

instance IsManaAbility (TriggeredAbility zone ot) where
  startingStageTrivial = StageFinal

instance IsZO zone ot => IsManaAbility (WithThisActivated zone ot) where
  startingStageTrivial = StageWithThisActivated

instance IsZO zone ot => IsManaAbility (WithThisTriggered zone ot) where
  startingStageTrivial = StageFinal

isManaAbility :: IsManaAbility a => a -> Bool
isManaAbility x = case isManaAbilityImpl x of
  IsManaAbility -> True
  _ -> False

type TrivialManaAbilityResult = Maybe (Maybe BasicLandType)

-- | A trivial mana ability is defined to be of the exact form
-- "{T}: Add {#} to your mana pool.", where (#) is a single non-variable mana.
isTrivialManaAbility :: forall a. IsManaAbility a => a -> Maybe (Maybe BasicLandType)
isTrivialManaAbility = isTrivialManaAbilityImpl (startingStageTrivial @a)

data Result :: Type where
  Indeterminate :: Result
  IsManaAbility :: Result
  IsNotManaAbility :: Result

instance Semigroup Result where
  (<>) x y = case (x, y) of
    (IsNotManaAbility, _) -> IsNotManaAbility
    (_, IsNotManaAbility) -> IsNotManaAbility
    (Indeterminate, _) -> y
    (_, Indeterminate) -> x
    (IsManaAbility, IsManaAbility) -> IsManaAbility

instance Monoid Result where
  mempty = Indeterminate

data StageTrivial
  = StageInitial
  | StageWithThisActivated
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

proxyThis :: forall zone ot. IsZO zone ot => ZO zone ot
proxyThis = ZO (singZone @zone) oThis
 where
  oThis = promoteIdToObjectN @ot proxyThisId

proxyYou :: ZOPlayer
proxyYou = ZO singZone oYou
 where
  oYou = promoteIdToObjectN proxyYouId

dummyZO :: forall zone ot. IsZO zone ot => ZO zone ot
dummyZO = ZO (singZone @zone) oThis
 where
  oThis = promoteIdToObjectN @ot $ ObjectId 0

mkDummyVar :: a -> Variable a
mkDummyVar = ReifiedVariable (VariableId 0)

class DummyVar a where
  dummyVar :: Variable a

instance DummyVar Int where
  dummyVar = mkDummyVar 0

instance DummyVar Power where
  dummyVar = mkDummyVar $ Power 0

instance (Typeable user, IsNat n) => DummyVar (Fin user n) where
  dummyVar = mkDummyVar FZ

instance IsManaAbilityImpl (Ability ot) where
  isManaAbilityImpl = \case
    Static{} -> IsNotManaAbility
    Activated withThis -> isManaAbilityImpl withThis
    Triggered withThis -> isManaAbilityImpl withThis
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageInitial, Activated withThis) -> isTrivialManaAbilityImpl StageWithThisActivated withThis
    _ -> Nothing

instance IsManaAbilityImpl (ActivatedAbility zone ot) where
  isManaAbilityImpl = \case
    Ability _cost elect -> isManaAbilityImpl elect
    Cycling{} -> IsNotManaAbility
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageFinal, Ability cost ability) -> case isTapThis cost of
      True -> isTrivialManaAbilityImpl StageFinal ability
      False -> Nothing
    _ -> Nothing

isTapThis :: IsOTN ot => Cost ot -> Bool
isTapThis = \case
  TapCost [Is this] -> getObjectId this == proxyThisId
  _ -> False

instance IsManaAbilityImpl x => IsManaAbilityImpl (Case x) where
  isManaAbilityImpl = \case
    CaseFin _var list -> isManaAbilityImpl list
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (Elect p el ot) where
  isManaAbilityImpl = \case
    ActivePlayer cont -> isManaAbilityImpl $ cont dummyZO
    All wmo -> isManaAbilityImpl wmo
    Choose _zo wmo -> isManaAbilityImpl wmo
    ChooseOption _zo _conds cont -> isManaAbilityImpl $ cont dummyVar
    Condition{} -> IsNotManaAbility
    ControllerOf _zo cont -> isManaAbilityImpl $ cont dummyZO
    Cost{} -> IsNotManaAbility
    Effect effects -> mconcat $ map isManaAbilityImpl effects
    Elect el -> isManaAbilityImpl el
    ElectActivated ability -> isManaAbilityImpl ability
    ElectCard{} -> IsNotManaAbility
    ElectCase case_ -> isManaAbilityImpl case_
    Event{} -> IsNotManaAbility
    If _cond then_ else_ -> isManaAbilityImpl then_ <> isManaAbilityImpl else_
    Listen listener -> isManaAbilityImpl listener
    OwnerOf _zo cont -> isManaAbilityImpl $ cont dummyZO
    Random wmo -> isManaAbilityImpl wmo
    Target{} -> IsNotManaAbility
    VariableFromPower _zo cont -> isManaAbilityImpl $ cont dummyVar
    VariableInt cont -> isManaAbilityImpl $ cont dummyVar
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageControllerOf, ControllerOf this cont) -> case getObjectId this == proxyThisId of
      True -> isTrivialManaAbilityImpl StageElectActivated $ cont proxyYou
      False -> Nothing
    (StageElectActivated, ElectActivated ability) -> isTrivialManaAbilityImpl StageFinal ability
    (StageFinal, Effect [e]) -> isTrivialManaAbilityImpl StageFinal e
    _ -> Nothing

instance IsManaAbilityImpl (Effect ef) where
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
  isTrivialManaAbilityImpl stage x = case (stage, x) of
    (StageFinal, AddMana you pool) -> case getObjectId you == proxyYouId of
      True -> extractSingleManaType pool
      False -> Nothing
    _ -> Nothing

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

instance IsManaAbilityImpl (Else el ot) where
  isManaAbilityImpl = \case
    ElseCost{} -> IsNotManaAbility
    ElseEffect elect -> isManaAbilityImpl elect
    ElseEvent -> Indeterminate
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl Event where
  isManaAbilityImpl _ = IsNotManaAbility
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl EventListener where
  isManaAbilityImpl = \case
    BecomesTapped wlo -> isManaAbilityImpl wlo
    EntersBattlefield wlo -> isManaAbilityImpl wlo
    EntersNonBattlefield wlo -> isManaAbilityImpl wlo
    Events listeners -> mconcat $ map isManaAbilityImpl listeners
    SpellIsCast wlo -> isManaAbilityImpl wlo
    TimePoint _timePoint elect -> isManaAbilityImpl elect
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl x => IsManaAbilityImpl (NatList user n x) where
  isManaAbilityImpl = \case
    LZ x -> isManaAbilityImpl x
    LS x xs -> isManaAbilityImpl x <> isManaAbilityImpl xs
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (TriggeredAbility zone ot) where
  isManaAbilityImpl = \case
    When elect -> isManaAbilityImpl elect
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsZone zone => IsManaAbilityImpl (WithLinkedObject zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Linked1 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked2 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked3 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked4 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked5 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsZone zone => IsManaAbilityImpl (WithMaskedObject zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Masked1 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked2 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked3 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked4 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked5 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked6 _reqs cont -> isManaAbilityImpl $ cont dummyZO
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl (WithMaskedObjects zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Maskeds1 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds2 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds3 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds4 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds5 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds6 _reqs cont -> isManaAbilityImpl $ cont $ List []
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsManaAbilityImpl ret => IsManaAbilityImpl (WithList ret zone ot) where
  isManaAbilityImpl = \case
    CountOf _list cont -> isManaAbilityImpl $ cont dummyVar
    Each _list cont -> isManaAbilityImpl $ cont dummyZO
    SuchThat _reqs withList -> isManaAbilityImpl withList
  isTrivialManaAbilityImpl _ _ = Nothing

instance IsZO zone ot => IsManaAbilityImpl (WithThisActivated zone ot) where
  isManaAbilityImpl = \case
    This1 cont -> isManaAbilityImpl $ cont dummyZO
    This2 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO)
    This3 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO)
    This4 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO, dummyZO)
    This5 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO, dummyZO, dummyZO)
  isTrivialManaAbilityImpl = \case
    StageWithThisActivated -> \case
      This1 cont -> isTrivialManaAbilityImpl StageControllerOf $ cont proxyThis
      This2 cont -> isTrivialManaAbilityImpl StageControllerOf $ cont (proxyThis, proxyThis)
      This3 cont -> isTrivialManaAbilityImpl StageControllerOf $ cont (proxyThis, proxyThis, proxyThis)
      This4 cont -> isTrivialManaAbilityImpl StageControllerOf $ cont (proxyThis, proxyThis, proxyThis, proxyThis)
      This5 cont -> isTrivialManaAbilityImpl StageControllerOf $ cont (proxyThis, proxyThis, proxyThis, proxyThis, proxyThis)
    _ -> const Nothing

instance IsZO zone ot => IsManaAbilityImpl (WithThisTriggered zone ot) where
  isManaAbilityImpl = \case
    This1 cont -> isManaAbilityImpl $ cont dummyZO
    This2 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO)
    This3 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO)
    This4 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO, dummyZO)
    This5 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO, dummyZO, dummyZO, dummyZO)
  isTrivialManaAbilityImpl _ _ = Nothing
