{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Phase (
  Phase (..),
  SPhase (..),
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Phase :: Type where
  BeginningPhase :: Phase
  PreCombatMainPhase :: Phase
  CombatPhase :: Phase
  PostCombatMainPhase :: Phase
  EndingPhase :: Phase
  deriving (Eq, Show, Typeable)

data SPhase :: Phase -> Type where
  SBeginningPhase :: SPhase 'BeginningPhase
  SPreCombatMainPhase :: SPhase 'PreCombatMainPhase
  SCombatPhase :: SPhase 'CombatPhase
  SPostCombatMainPhase :: SPhase 'PostCombatMainPhase
  SEndingPhase :: SPhase 'EndingPhase
  deriving (Typeable)

deriving instance Eq (SPhase a)

deriving instance Ord (SPhase a)

deriving instance Show (SPhase a)

instance ConsIndex Phase where
  consIndex = \case
    BeginningPhase -> 1
    PreCombatMainPhase -> 2
    CombatPhase -> 3
    PostCombatMainPhase -> 4
    EndingPhase -> 5
