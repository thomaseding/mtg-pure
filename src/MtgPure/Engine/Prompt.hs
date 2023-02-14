{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (..),
  ActivateAbility,
  ActivateResult (..),
  AttackingPlayer (..),
  CardCount (..),
  CardIndex (..),
  CastSpell,
  DeclaredAttacker (..),
  DeclaredBlocker (..),
  DefendingPlayer (..),
  InternalLogicError (..),
  InvalidCastSpell (..),
  InvalidPlayLand (..),
  OwnedCard (..),
  PlayerCount (..),
  PlayerIndex (..),
  PlayLand,
  PriorityAction (..),
  Prompt' (..),
  QueryObjectResult (..),
  RelativeAbilityIndex (..),
  SomeActivatedAbility (..),
  SpecialAction (..),
  PendingReady (..),
  Pending,
  Ready,
  Elected (..),
  electedObject_controller,
  electedObject_cost,
  AnyElected (..),
  ResolveElected (..),
  SourceZO (..),
  Ev (..),
  TriggerTime (..),
) where

import safe Control.Monad.Util (Attempt)
import safe Data.Kind (Type)
import safe Data.List.NonEmpty (NonEmpty)
import safe Data.Nat (Fin, IsNat, NatList)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Engine.Monad (CallFrameInfo)
import safe MtgPure.Engine.Orphans.ZO ()
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Mana.Mana (Mana)
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost)
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool, ManaPayment)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCreature, OTNLand, OTNPermanent, OTNPlayerPlaneswalker, OTNSpell)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId (getUntypedObject), ObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.Player (Player)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (AnyCard, AnyToken, CardFacet, Cost, Effect, Elect, WithThisActivated)
import safe MtgPure.Model.Recursive.Ord ()
import safe qualified MtgPure.Model.Recursive.Ord as O
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Variable (Var (..))
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO)

data InternalLogicError :: Type where
  CantHappenByConstruction :: InternalLogicError -- TODO: ditch this for informative constructors
  CorruptCallStackLogging :: InternalLogicError
  ExpectedCardToBeAPermanentCard :: InternalLogicError
  ExpectedStackObjectToExist :: IsZO zone ot => ZO zone ot -> InternalLogicError
  GameShouldHaveEndedBecauseThereIsOnlyOnePlayerLeft :: InternalLogicError
  InvalidPermanent :: ZO 'ZBattlefield OTNPermanent -> InternalLogicError
  InvalidPlayer :: Object 'OTPlayer -> InternalLogicError
  ManaAbilitiesDontHaveTargetsSoNoZoShouldBeNeeded :: InternalLogicError
  NotSureWhatThisEntails :: InternalLogicError
  ObjectDoesNotHaveAbility :: IsZO zone ot => SomeActivatedAbility zone ot -> InternalLogicError
  ObjectIdExistsAndAlsoDoesNotExist :: IsZO zone ot => ZO zone ot -> InternalLogicError
  deriving (Typeable)

deriving instance Show InternalLogicError

newtype PlayerCount = PlayerCount {unPlayerCount :: Int}
  deriving (Eq, Ord, Show, Typeable)

newtype PlayerIndex = PlayerIndex {unPlayerIndex :: Int}
  deriving (Eq, Ord, Show, Typeable)

newtype CardCount = CardCount {unCardCount :: Int}
  deriving (Eq, Ord, Show, Typeable)

newtype CardIndex = CardIndex {unCardIndex :: Int}
  deriving (Eq, Ord, Show, Typeable)

newtype RelativeAbilityIndex = RelativeAbilityIndex {unRelativeAbilityIndex :: Int}
  deriving (Eq, Ord, Show, Typeable)

data OwnedCard = OwnedCard (Object 'OTPlayer) AnyCard

data ActivateResult :: Type where
  IllegalActivation :: ActivateResult
  ActivatedManaAbility :: [Ev] -> ActivateResult
  ActivatedNonManaAbility :: ActivateResult

data DeclaredAttacker = DeclaredAttacker
  { declaredAttacker_attacker :: ZO 'ZBattlefield OTNCreature
  , declaredAttacker_victim :: ZO 'ZBattlefield OTNPlayerPlaneswalker
  }
  deriving (Eq, Ord, Show)

data DeclaredBlocker = DeclaredBlocker
  { declaredBlocker_blocker :: ZO 'ZBattlefield OTNCreature
  , declaredBlocker_attackers :: NonEmpty (ZO 'ZBattlefield OTNCreature)
  }
  deriving (Eq, Ord, Show)

data QueryObjectResult = QueryObjectResult
  { qor_ :: ()
  , qorCard :: Maybe AnyCard
  , qorToken :: Maybe AnyToken
  , qorController :: Object 'OTPlayer
  , qorOwner :: Object 'OTPlayer
  , qorPermanent :: Maybe Permanent
  , qorPlayer :: Maybe Player
  , qorZone :: Zone
  }

data PendingReady (p :: PrePost) (el :: Type) (ot :: Type) where
  Pending :: {unPending :: Elect 'Post el ot} -> Pending el ot
  Ready :: {unReady :: el} -> Ready el ot

deriving instance Show el => Show (PendingReady p el ot)

type Pending = PendingReady 'Pre

type Ready = PendingReady 'Post

data Elected (pEffect :: PrePost) (ot :: Type) :: Type where
  ElectedActivatedAbility ::
    IsZO zone ot =>
    { electedActivatedAbility_ability :: SomeActivatedAbility zone ot
    , electedActivatedAbility_controller :: Object 'OTPlayer
    , electedActivatedAbility_this :: ZO zone ot
    , electedActivatedAbility_cost :: Cost ot
    , electedActivatedAbility_effect :: PendingReady pEffect (Effect 'OneShot) ot
    } ->
    Elected pEffect ot
  ElectedSpell ::
    IsZO zone OTNSpell =>
    { -- | NOTE: This is the card that was cast, so its lifetime is short and the object it points to is often dead.
      -- For example this is the hand card that was cast, but once it is put on the stack it is no longer in the hand
      -- and the object it points to is then dead.
      electedSpell_originalSource :: ZO zone OTNSpell
    , electedSpell_controller :: Object 'OTPlayer
    , electedSpell_card :: AnyCard -- TODO: OwnedCard?
    , electedSpell_facet :: CardFacet ot
    , electedSpell_cost :: Cost ot
    , electedSpell_effect :: Maybe (PendingReady pEffect (Effect 'OneShot) ot)
    } ->
    Elected pEffect ot
  deriving (Typeable)

electedObject_controller :: Elected pEffect ot -> Object 'OTPlayer
electedObject_controller elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_controller
  ElectedSpell{} -> electedSpell_controller

electedObject_cost :: Elected pEffect ot -> Cost ot
electedObject_cost elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_cost
  ElectedSpell{} -> electedSpell_cost

data AnyElected (pEffect :: PrePost) :: Type where
  AnyElected :: IsOTN ot => Elected pEffect ot -> AnyElected pEffect
  deriving (Typeable)

newtype AttackingPlayer = AttackingPlayer {unAttackingPlayer :: Object 'OTPlayer}
  deriving (Eq, Ord, Show)

newtype DefendingPlayer = DefendingPlayer {unDefendingPlayer :: Object 'OTPlayer}
  deriving (Eq, Ord, Show)

data Prompt' (opaqueGameState :: (Type -> Type) -> Type) (m :: Type -> Type) (magic :: Type -> Type) = Prompt
  { exceptionCantBeginGameWithoutPlayers :: m ()
  , exceptionInvalidCastSpell :: opaqueGameState m -> Object 'OTPlayer -> InvalidCastSpell -> m ()
  , exceptionInvalidGenericManaPayment :: Mana 'NoVar 'NonSnow 'Ty1 -> CompleteManaPool -> m ()
  , exceptionInvalidPlayLand :: opaqueGameState m -> Object 'OTPlayer -> InvalidPlayLand -> m ()
  , exceptionInvalidShuffle :: CardCount -> [CardIndex] -> m ()
  , exceptionInvalidStartingPlayer :: PlayerCount -> PlayerIndex -> m ()
  , exceptionZoneObjectDoesNotExist :: forall zone ot. IsZO zone ot => ZO zone ot -> m ()
  , promptChooseAttackers :: Attempt -> opaqueGameState m -> AttackingPlayer -> magic [DeclaredAttacker]
  , promptChooseBlockers :: Attempt -> opaqueGameState m -> DefendingPlayer -> NonEmpty DeclaredAttacker -> magic [DeclaredBlocker]
  , promptChooseOption :: forall user n elem. (Typeable user, IsNat n) => opaqueGameState m -> Object 'OTPlayer -> NatList user n elem -> m (Fin user n)
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: Attempt -> PlayerCount -> magic PlayerIndex
  , promptLogCallPop :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptLogCallPush :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptPayDynamicMana :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> DynamicManaCost 'NoVar -> magic ManaPayment
  , promptPerformMulligan :: Attempt -> Object 'OTPlayer -> [AnyCard] -> magic Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Attempt -> opaqueGameState m -> Object 'OTPlayer -> NonEmpty (ZO zone ot) -> magic (ZO zone ot)
  , promptPriorityAction :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> magic (PriorityAction ())
  , promptShuffle :: Attempt -> CardCount -> Object 'OTPlayer -> magic [CardIndex]
  }

data AbsoluteActivatedAbilityIndex :: Type where
  AbsoluteActivatedAbilityIndex :: ObjectId -> RelativeAbilityIndex -> AbsoluteActivatedAbilityIndex
  deriving (Eq, Ord, Show, Typeable)

instance GetObjectId AbsoluteActivatedAbilityIndex where
  getUntypedObject (AbsoluteActivatedAbilityIndex i _) = getUntypedObject i

data SomeActivatedAbility (zone :: Zone) (ot :: Type) :: Type where
  SomeActivatedAbility ::
    -- NOTE: Escaped `ot'` is here because modelled Activated abilities are indexed by the leaf object type, not the composite.
    (IsZO zone ot, IsZO zone ot') =>
    { someActivatedZO :: ZO zone ot
    , someActivatedAbility :: WithThisActivated zone ot'
    } ->
    SomeActivatedAbility zone ot

deriving instance Show (SomeActivatedAbility zone ot)

instance Eq (SomeActivatedAbility zone ot) where
  (==) x y = O.runEnvM (ordSomeActivatedAbility x y) == EQ

instance Ord (SomeActivatedAbility zone ot) where
  compare x y = O.runEnvM (ordSomeActivatedAbility x y)

ordSomeActivatedAbility ::
  forall zone ot.
  SomeActivatedAbility zone ot ->
  SomeActivatedAbility zone ot ->
  O.EnvM Ordering
ordSomeActivatedAbility x = case x of
  SomeActivatedAbility zo1 withThis1 -> \case
    SomeActivatedAbility zo2 withThis2' ->
      let go ::
            forall ot1 ot2.
            IsZO zone ot1 =>
            IsZO zone ot2 =>
            WithThisActivated zone ot1 ->
            WithThisActivated zone ot2 ->
            O.EnvM Ordering
          go _ _ = case cast withThis2' of
            Nothing -> O.compareOT @ot1 @ot2
            Just withThis2 ->
              O.seqM
                [ O.ordZoneObject zo1 zo2
                , O.ordWithThisActivated withThis1 withThis2
                ]
       in go withThis1 withThis2'

-- | This is a phantom type.
data ActivateAbility

-- | This is a phantom type.
data CastSpell

-- | This is a phantom type.
data PlayLand

data PriorityAction (a :: Type) :: Type where
  ActivateAbility :: IsZO zone ot => SomeActivatedAbility zone ot -> PriorityAction ActivateAbility
  AskPriorityActionAgain :: Maybe Attempt -> PriorityAction () -- NOTE: This is handy for client code.
  -- NOTE (305.9): Lands + other types can never be cast
  -- Unfortunately OTNSpell intersects OTArtifactLand. Such is life.
  -- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
  -- though it's probably unnecessary for Authoring (thankfully).
  CastSpell :: IsZO zone OTNSpell => ZO zone OTNSpell -> PriorityAction CastSpell
  -- | In genuine MTG, this can be done at any time, but we're going to restrict its use to here.
  -- See doc notes on `endTheGame` for more details on why.
  Concede :: PriorityAction ()
  PassPriority :: PriorityAction ()
  PriorityAction :: PriorityAction a -> PriorityAction ()
  SpecialAction :: SpecialAction a -> PriorityAction a

deriving instance Show (PriorityAction a)

data SpecialAction (a :: Type) :: Type where
  PlayLand :: IsZO zone OTNLand => ZO zone OTNLand -> SpecialAction PlayLand

deriving instance Show (SpecialAction a)

data InvalidPlayLand :: Type where
  PlayLand_AtMaxLands :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_CannotPlayFromZone :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NoPriority :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NotActive :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NotALand :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NotInZone :: ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NotMainPhase :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_NotOwned :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand
  PlayLand_StackNonEmpty :: IsZone zone => ZO zone OTNLand -> InvalidPlayLand

deriving instance Show InvalidPlayLand

data InvalidCastSpell :: Type where
  CastSpell_CannotPlayFromZone :: IsZone zone => ZO zone OTNSpell -> InvalidCastSpell
  CastSpell_NoPriority :: IsZone zone => ZO zone OTNSpell -> InvalidCastSpell
  CastSpell_NotASpell :: IsZone zone => ZO zone OTNSpell -> InvalidCastSpell
  CastSpell_NotInZone :: ZO zone OTNSpell -> InvalidCastSpell
  CastSpell_NotOwned :: ZO zone OTNSpell -> InvalidCastSpell

deriving instance Show InvalidCastSpell

data ResolveElected :: Type where
  Fizzled :: ResolveElected
  ResolvedEffect :: [Ev] -> ResolveElected
  PermanentResolved :: ResolveElected

data SourceZO :: Type where
  SourceZO :: IsZO zone ot => ZO zone ot -> SourceZO

data TriggerTime :: Type where
  DuringResolution :: TriggerTime
  AfterResolution :: TriggerTime

-- | "Ev" is short for "Event". (Avoids name conflict with Model.Event)
data Ev :: Type where
  -- | `Just source` is the spell or effect that caused the tapping to happen.
  -- It's `Nothing` when the game state does it on its own, such as untap step.
  EvTapped :: Maybe SourceZO -> ZO 'ZBattlefield OTNPermanent -> Ev
  -- | `Just source` is the spell or effect that caused the untapping to happen.
  -- It's `Nothing` when the game state does it on its own, such as untap step.
  EvUntapped :: Maybe SourceZO -> ZO 'ZBattlefield OTNPermanent -> Ev
  EvEndTheTurn :: Maybe SourceZO -> Ev
