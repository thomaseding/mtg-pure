{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.IsManaAbility (
  IsManaAbility (..),
) where

import safe Data.Kind (Type)
import safe Data.Nat (Fin (..), IsNat, NatList (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.ObjectId (ObjectId (..))
import safe MtgPure.Model.Object.PromoteIdToObjectN (PromoteIdToObjectN (..))
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  Case (..),
  Effect (..),
  Elect (..),
  Else (..),
  Event,
  EventListener,
  EventListener' (..),
  List (..),
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
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO, ZoneObject (ZO))

-- For example cards with abilities that generate mana but are not mana abilities, see:
-- https://boardgames.stackexchange.com/questions/44040/does-mtg-have-a-card-with-a-mana-generating-ability-that-has-a-target-and-is-thu

class IsManaAbilityImpl a => IsManaAbility a where
  isManaAbility :: a -> Bool
  isManaAbility x = case isManaAbilityImpl x of
    IsManaAbility -> True
    _ -> False

instance IsManaAbility (Ability ot)

instance IsManaAbility (ActivatedAbility zone ot)

instance IsManaAbility (TriggeredAbility zone ot)

instance IsZO zone ot => IsManaAbility (WithThisActivated zone ot)

instance IsZO zone ot => IsManaAbility (WithThisTriggered zone ot)

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

class IsManaAbilityImpl a where
  isManaAbilityImpl :: a -> Result

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

instance IsManaAbilityImpl (ActivatedAbility zone ot) where
  isManaAbilityImpl = \case
    Ability _cost elect -> isManaAbilityImpl elect
    Cycling{} -> IsNotManaAbility

instance IsManaAbilityImpl x => IsManaAbilityImpl (Case x) where
  isManaAbilityImpl = \case
    CaseFin _var list -> isManaAbilityImpl list

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

instance IsManaAbilityImpl (Else el ot) where
  isManaAbilityImpl = \case
    ElseCost{} -> IsNotManaAbility
    ElseEffect elect -> isManaAbilityImpl elect
    ElseEvent -> Indeterminate

instance IsManaAbilityImpl Event where
  isManaAbilityImpl _ = IsNotManaAbility

instance IsManaAbilityImpl EventListener where
  isManaAbilityImpl = \case
    BecomesTapped wlo -> isManaAbilityImpl wlo
    EntersBattlefield wlo -> isManaAbilityImpl wlo
    EntersNonBattlefield wlo -> isManaAbilityImpl wlo
    Events listeners -> mconcat $ map isManaAbilityImpl listeners
    SpellIsCast wlo -> isManaAbilityImpl wlo
    TimePoint _timePoint elect -> isManaAbilityImpl elect

instance IsManaAbilityImpl x => IsManaAbilityImpl (NatList user n x) where
  isManaAbilityImpl = \case
    LZ x -> isManaAbilityImpl x
    LS x xs -> isManaAbilityImpl x <> isManaAbilityImpl xs

instance IsManaAbilityImpl (TriggeredAbility zone ot) where
  isManaAbilityImpl = \case
    When elect -> isManaAbilityImpl elect

instance IsZone zone => IsManaAbilityImpl (WithLinkedObject zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Linked1 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked2 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked3 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked4 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Linked5 _wit _reqs cont -> isManaAbilityImpl $ cont dummyZO

instance IsZone zone => IsManaAbilityImpl (WithMaskedObject zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Masked1 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked2 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked3 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked4 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked5 _reqs cont -> isManaAbilityImpl $ cont dummyZO
    Masked6 _reqs cont -> isManaAbilityImpl $ cont dummyZO

instance IsManaAbilityImpl (WithMaskedObjects zone (Elect p el) ot) where
  isManaAbilityImpl = \case
    Maskeds1 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds2 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds3 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds4 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds5 _reqs cont -> isManaAbilityImpl $ cont $ List []
    Maskeds6 _reqs cont -> isManaAbilityImpl $ cont $ List []

instance IsManaAbilityImpl ret => IsManaAbilityImpl (WithList ret zone ot) where
  isManaAbilityImpl = \case
    CountOf _list cont -> isManaAbilityImpl $ cont dummyVar
    Each _list cont -> isManaAbilityImpl $ cont dummyZO
    SuchThat _reqs withList -> isManaAbilityImpl withList

instance IsZO zone ot => IsManaAbilityImpl (WithThisActivated zone ot) where
  isManaAbilityImpl = \case
    This1 cont -> isManaAbilityImpl $ cont dummyZO
    This2 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO)

instance IsZO zone ot => IsManaAbilityImpl (WithThisTriggered zone ot) where
  isManaAbilityImpl = \case
    This1 cont -> isManaAbilityImpl $ cont dummyZO
    This2 cont -> isManaAbilityImpl $ cont (dummyZO, dummyZO)
