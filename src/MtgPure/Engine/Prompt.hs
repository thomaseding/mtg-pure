{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Prompt (
  InternalLogicError (..),
  Prompt' (..),
  PlayerCount (..),
  PlayerIndex (..),
  CardCount (..),
  CardIndex (..),
  RelativeAbilityIndex (..),
  AbsoluteActivatedAbilityIndex (..),
  Play (..),
  SpecialAction (..),
  InvalidPlayLand (..),
  InvalidCastSpell (..),
  CallFrameId,
  CallFrameInfo (..),
  SomeActivatedAbility (..),
) where

import safe Control.Monad.Util (Attempt)
import safe Data.Kind (Type)
import safe Data.List.NonEmpty (NonEmpty)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Engine.Orphans.ZO ()
import safe MtgPure.Model.Object.OTKind (OTActivatedAbility, OTLand, OTPermanent, OTSpell)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId (getUntypedObject), ObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Recursive (AnyCard, WithThisActivated)
import safe MtgPure.Model.Recursive.Ord ()
import safe qualified MtgPure.Model.Recursive.Ord as O
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

data InternalLogicError :: Type where
  CantHappenByConstruction :: InternalLogicError
  CorruptCallStackLogging :: InternalLogicError
  ExpectedCardToBeAPermanentCard :: InternalLogicError
  ExpectedStackObjectToExist :: IsZO zone ot => ZO zone ot -> InternalLogicError
  ImpossibleGameOver :: InternalLogicError
  InvalidPermanent :: ZO 'ZBattlefield OTPermanent -> InternalLogicError
  InvalidPlayer :: Object 'OTPlayer -> InternalLogicError
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

type CallFrameId = Int

data CallFrameInfo = CallFrameInfo
  { callFrameId :: CallFrameId
  , callFrameName :: String
  }
  deriving (Eq, Ord, Show)

data Prompt' (opaqueGameState :: (Type -> Type) -> Type) (m :: Type -> Type) = Prompt
  { exceptionCantBeginGameWithoutPlayers :: m ()
  , exceptionInvalidCastSpell :: opaqueGameState m -> Object 'OTPlayer -> InvalidCastSpell -> m ()
  , exceptionInvalidPlayLand :: opaqueGameState m -> Object 'OTPlayer -> InvalidPlayLand -> m ()
  , exceptionInvalidShuffle :: CardCount -> [CardIndex] -> m ()
  , exceptionInvalidStartingPlayer :: PlayerCount -> PlayerIndex -> m ()
  , exceptionZoneObjectDoesNotExist :: forall zone ot. IsZO zone ot => ZO zone ot -> m ()
  , promptActivateAbility :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> m (Maybe (Play OTActivatedAbility))
  , promptCastSpell :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> m (Maybe (Play OTSpell))
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: Attempt -> PlayerCount -> m PlayerIndex
  , promptLogCallPop :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptLogCallPush :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptPerformMulligan :: Attempt -> Object 'OTPlayer -> [AnyCard] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Attempt -> opaqueGameState m -> Object 'OTPlayer -> NonEmpty (ZO zone ot) -> m (ZO zone ot)
  , promptPlayLand :: Attempt -> opaqueGameState m -> Object 'OTPlayer -> m (Maybe (Play OTLand))
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

data Play (ot :: Type) :: Type where
  ActivateAbility :: IsZO zone ot => SomeActivatedAbility zone ot -> Play OTActivatedAbility
  -- NOTE (305.9): Lands + other types can never be cast
  -- Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
  -- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
  -- though it's probably unnecessary for Authoring (thankfully).
  CastSpell :: IsZO zone OTSpell => ZO zone OTSpell -> Play OTSpell
  PlayLand :: IsZO zone OTLand => ZO zone OTLand -> Play OTLand

data SpecialAction :: Type where
  SA_PlayLand :: Play OTLand -> SpecialAction

data InvalidPlayLand :: Type where
  PlayLand_AtMaxLands :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_CannotPlayFromZone :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NoPriority :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotActive :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotALand :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotInZone :: ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotMainPhase :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotOwned :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_StackNonEmpty :: IsZone zone => ZO zone OTLand -> InvalidPlayLand

deriving instance Show InvalidPlayLand

data InvalidCastSpell :: Type where
  CastSpell_CannotPlayFromZone :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NoPriority :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotASpell :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotInZone :: ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotOwned :: ZO zone OTSpell -> InvalidCastSpell

deriving instance Show InvalidCastSpell
