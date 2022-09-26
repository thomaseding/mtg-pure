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
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object (Object, ObjectType (..))
import safe MtgPure.Model.ObjectType.Card (WCard)
import safe MtgPure.Model.ObjectType.Kind (OTLand, OTPermanent, OTSpell)
import safe MtgPure.Model.Recursive (ActivatedAbility, Card, SomeCard)
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)

data InternalLogicError
  = ExpectedCardToBeAPermanentCard
  | ExpectedStackObjectToExist
  | InvalidPermanent (ZO 'ZBattlefield OTPermanent)
  | InvalidPlayer (Object 'OTPlayer)
  | ImpossibleGameOver
  | NotSureWhatThisEntails
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
  , promptPerformMulligan :: Object 'OTPlayer -> [Card ()] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Object 'OTPlayer -> [ZO zone ot] -> m (ZO zone ot)
  , promptPlayLand :: opaqueGameState m -> Object 'OTPlayer -> m (Maybe PlayLand)
  , promptShuffle :: CardCount -> Object 'OTPlayer -> m [CardIndex]
  }

data ActivateAbility :: Type where
  ActivateAbility :: WCard ot -> ZO zone ot -> ActivatedAbility zone ot -> ActivateAbility

-- NB (305.9): Lands + other types can never be cast
-- Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
-- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
-- though it's probably unnecessary for Authoring (thankfully).
newtype CastSpell :: Type where
  CastSpell :: SomeCard OTSpell -> CastSpell

data PlayLand :: Type where
  PlayLand :: IsZone zone => ZO zone OTLand -> PlayLand

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
  CastSpell_NotASpell :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_CannotPlayFromZone :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
