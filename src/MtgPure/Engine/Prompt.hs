{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
  ActivateAbility (..),
  CastSpell (..),
  PlayLand (..),
  SpecialAction (..),
  InvalidPlayLand (..),
  InvalidCastSpell (..),
  ShowZO (..),
  CallFrameId,
  CallFrameInfo (..),
  SomeActivatedAbility (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object (Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectType.Kind (OTLand, OTPermanent, OTSpell)
import safe MtgPure.Model.Recursive (AnyCard, WithThisActivated)
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)

newtype ShowZO zone ot = ShowZO (ZO zone ot)
  deriving (Eq, Ord)

instance GetObjectId (ObjectN ot) => GetObjectId (ShowZO zone ot) where
  getObjectId (ShowZO zo) = getObjectId zo

instance GetObjectId (ObjectN ot) => Show (ShowZO zone ot) where
  show zo = "ZO=" ++ show n
   where
    ObjectId n = getObjectId zo

data InternalLogicError
  = CantHappenByConstruction
  | CorruptCallStackLogging
  | ExpectedCardToBeAPermanentCard
  | ExpectedStackObjectToExist
  | ImpossibleGameOver
  | InvalidPermanent (ShowZO 'ZBattlefield OTPermanent)
  | InvalidPlayer (Object 'OTPlayer)
  | NotSureWhatThisEntails
  | ObjectIdExistsAndAlsoDoesNotExist
  deriving (Typeable)

deriving instance Eq InternalLogicError

deriving instance Ord InternalLogicError

deriving instance Show InternalLogicError

newtype PlayerCount = PlayerCount {unPlayerCount :: Int}
  deriving (Eq, Ord, Show)

newtype PlayerIndex = PlayerIndex {unPlayerIndex :: Int}
  deriving (Eq, Ord, Show)

newtype CardCount = CardCount {unCardCount :: Int}
  deriving (Eq, Ord, Show)

newtype CardIndex = CardIndex {unCardIndex :: Int}
  deriving (Eq, Ord, Show)

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
  , promptActivateAbility :: opaqueGameState m -> Object 'OTPlayer -> m (Maybe ActivateAbility)
  , promptCastSpell :: opaqueGameState m -> Object 'OTPlayer -> m (Maybe CastSpell)
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: PlayerCount -> m PlayerIndex
  , promptLogCallPop :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptLogCallPush :: opaqueGameState m -> CallFrameInfo -> m ()
  , promptPerformMulligan :: Object 'OTPlayer -> [AnyCard] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Object 'OTPlayer -> [ZO zone ot] -> m (ZO zone ot) -- TODO: NonEmpty; TODO: Add a discriminant for UX
  , promptPlayLand :: opaqueGameState m -> Object 'OTPlayer -> m (Maybe PlayLand)
  , promptShuffle :: CardCount -> Object 'OTPlayer -> m [CardIndex]
  }

data SomeActivatedAbility (zone :: Zone) (ot :: Type) :: Type where
  SomeActivatedAbility ::
    (IsZO zone ot, IsZO zone ot') =>
    { someActivatedZO :: ZO zone ot
    , someActivatedAbility :: WithThisActivated zone ot'
    } ->
    SomeActivatedAbility zone ot

data ActivateAbility :: Type where
  ActivateAbility :: IsZO zone ot => SomeActivatedAbility zone ot -> ActivateAbility

-- NB (305.9): Lands + other types can never be cast
-- Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
-- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
-- though it's probably unnecessary for Authoring (thankfully).
data CastSpell :: Type where
  CastSpell :: IsZO zone OTSpell => ZO zone OTSpell -> CastSpell

data PlayLand :: Type where
  PlayLand :: IsZO zone OTLand => ZO zone OTLand -> PlayLand

data SpecialAction :: Type where
  SA_PlayLand :: PlayLand -> SpecialAction

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

data InvalidCastSpell :: Type where
  CastSpell_CannotPlayFromZone :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NoPriority :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotASpell :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotInZone :: ZO zone OTSpell -> InvalidCastSpell
  CastSpell_NotOwned :: ZO zone OTSpell -> InvalidCastSpell
