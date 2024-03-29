{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Step (
  Step (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Phase (Phase (..))

data Step :: Phase -> Type where
  UntapStep :: Step 'BeginningPhase
  UpkeepStep :: Step 'BeginningPhase
  DrawStep :: Step 'BeginningPhase
  BeginningOfCombatStep :: Step 'CombatPhase
  DeclareAttackersStep :: Step 'CombatPhase
  DeclareBlockersStep :: Step 'CombatPhase
  CombatDamageStep :: Step 'CombatPhase
  EndOfCombatStep :: Step 'CombatPhase
  EndStep :: Step 'EndingPhase
  CleanupStep :: Step 'EndingPhase
  deriving (Typeable)

deriving instance Eq (Step p)

deriving instance Ord (Step p)

deriving instance Show (Step p)
