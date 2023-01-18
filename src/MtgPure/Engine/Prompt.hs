{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (..),
  ActivateAbility,
  ActivateResult (..),
  CallFrameId,
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  CastSpell,
  DeclaredAttacker (..),
  DeclaredBlocker (..),
  EnactInfo (..),
  InternalLogicError (..),
  InvalidCastSpell (..),
  InvalidPlayLand (..),
  OwnedCard (..),
  PlayerCount (..),
  PlayerIndex (..),
  PlayLand,
  PriorityAction (..),
  Prompt' (..),
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
) where

import safe Control.Monad.Util (Attempt)
import safe Data.Kind (Type)
import safe Data.List.NonEmpty (NonEmpty)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Engine.Orphans.ZO ()
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Mana.Mana (Mana)
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (NonSnow))
import safe MtgPure.Model.Object.OTNAliases (OTNCreature, OTNLand, OTNPermanent, OTNPlayerPlaneswalker, OTNSpell)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId (getUntypedObject), ObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (AnyCard, CardFacet, Cost, Effect, Elect, WithThisActivated)
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

type CallFrameId = Int

data CallFrameInfo = CallFrameInfo
  { callFrameId :: CallFrameId
  , callFrameName :: String
  }
  deriving (Eq, Ord, Show)

data ActivateResult :: Type where
  IllegalActivation :: ActivateResult
  ActivatedManaAbility :: EnactInfo -> ActivateResult
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

data PendingReady (p :: PrePost) (el :: Type) (ot :: Type) where
  Pending :: {unPending :: Elect 'Post el ot} -> Pending el ot
  Ready :: {unReady :: el} -> Ready el ot

deriving instance Show el => Show (PendingReady p el ot)

type Pending = PendingReady 'Pre

type Ready = PendingReady 'Post

data Elected (pEffect :: PrePost) (ot :: Type) :: Type where
  ElectedActivatedAbility ::
    IsZO zone ot =>
    { electedActivatedAbility_controller :: Object 'OTPlayer
    , electedActivatedAbility_this :: ZO zone ot
    , electedActivatedAbility_cost :: Cost ot
    , electedActivatedAbility_effect :: PendingReady pEffect (Effect 'OneShot) ot
    } ->
    Elected pEffect ot
  ElectedSpell ::
    { electedSpell_controller :: Object 'OTPlayer
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

data Prompt' (opaqueGameState :: (Type -> Type) -> Type) (m :: Type -> Type) = Prompt
  { exceptionCantBeginGameWithoutPlayers :: m ()
  , exceptionInvalidCastSpell :: opaqueGameState m -> Object 'OTPlayer -> InvalidCastSpell -> m ()
  , exceptionInvalidGenericManaPayment :: Mana 'NoVar 'NonSnow 'MTGeneric -> CompleteManaPool -> m ()
  , exceptionInvalidPlayLand :: opaqueGameState m -> Object 'OTPlayer -> InvalidPlayLand -> m ()
  , exceptionInvalidShuffle :: CardCount -> [CardIndex] -> m ()
  , exceptionInvalidStartingPlayer :: PlayerCount -> PlayerIndex -> m ()
  , exceptionZoneObjectDoesNotExist :: forall zone ot. IsZO zone ot => ZO zone ot -> m ()
  , promptChooseAttackers :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> m [DeclaredAttacker]
  , promptChooseBlockers :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> NonEmpty DeclaredAttacker -> m [DeclaredBlocker]
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: Attempt -> PlayerCount -> m PlayerIndex
  , promptLogCallPop :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptLogCallPush :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptPayGeneric :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> Mana 'NoVar 'NonSnow 'MTGeneric -> m CompleteManaPool
  , promptPerformMulligan :: Attempt -> Object 'OTPlayer -> [AnyCard] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Attempt -> opaqueGameState m -> Object 'OTPlayer -> NonEmpty (ZO zone ot) -> m (ZO zone ot)
  , promptPriorityAction :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> m (PriorityAction ())
  , promptShuffle :: Attempt -> CardCount -> Object 'OTPlayer -> m [CardIndex]
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
  AskPriorityActionAgain :: PriorityAction () -- NOTE: This is handy for client code.
  -- NOTE (305.9): Lands + other types can never be cast
  -- Unfortuantely OTNSpell intersects OTArtifactLand. Such is life.
  -- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
  -- though it's probably unnecessary for Authoring (thankfully).
  CastSpell :: IsZO zone OTNSpell => ZO zone OTNSpell -> PriorityAction CastSpell
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

data EnactInfo = EnactInfo
  { enactInfo_ :: ()
  , enactInfo_becameTapped :: [ZO 'ZBattlefield OTNPermanent]
  , enactInfo_becameUntapped :: [ZO 'ZBattlefield OTNPermanent]
  , enactInfo_endTheTurn :: Bool
  }
  deriving (Show)

instance Semigroup EnactInfo where
  x <> y =
    EnactInfo
      { enactInfo_ = ()
      , enactInfo_becameTapped = becameTapped1 <> becameTapped2
      , enactInfo_becameUntapped = becameUntapped1 <> becameUntapped2
      , enactInfo_endTheTurn = endTheTurn1 || endTheTurn2
      }
   where
    EnactInfo
      { enactInfo_ = ()
      , enactInfo_becameTapped = becameTapped1
      , enactInfo_becameUntapped = becameUntapped1
      , enactInfo_endTheTurn = endTheTurn1
      } = x
    EnactInfo
      { enactInfo_ = ()
      , enactInfo_becameTapped = becameTapped2
      , enactInfo_becameUntapped = becameUntapped2
      , enactInfo_endTheTurn = endTheTurn2
      } = y

instance Monoid EnactInfo where
  mempty =
    EnactInfo
      { enactInfo_ = ()
      , enactInfo_becameTapped = []
      , enactInfo_becameUntapped = []
      , enactInfo_endTheTurn = False
      }

data ResolveElected :: Type where
  Fizzled :: ResolveElected
  ResolvedEffect :: EnactInfo -> ResolveElected
  PermanentResolved :: ResolveElected
