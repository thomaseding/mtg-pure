{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.PhaseStep (
  PhaseStep (..),
  isMainPhase,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Phase (Phase (..))
import safe MtgPure.Model.Step (Step)

data PhaseStep :: Type where
  PSBeginningPhase :: Step 'BeginningPhase -> PhaseStep
  PSPreCombatMainPhase :: PhaseStep
  PSCombatPhase :: Step 'CombatPhase -> PhaseStep
  PSPostCombatMainPhase :: PhaseStep
  PSEndingPhase :: Step 'EndingPhase -> PhaseStep
  deriving (Eq, Ord, Typeable)

isMainPhase :: PhaseStep -> Bool
isMainPhase = \case
  PSPreCombatMainPhase -> True
  PSPostCombatMainPhase -> True
  _ -> False
