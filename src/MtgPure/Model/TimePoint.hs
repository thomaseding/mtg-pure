{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.TimePoint (
  TimePoint (..),
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Phase (Phase, SPhase)
import safe MtgPure.Model.Step (Step)

data TimePoint (p :: Phase) :: Type where
  PhaseBegin :: SPhase a -> TimePoint a
  PhaseEnd :: SPhase a -> TimePoint a
  StepBegin :: Step a -> TimePoint a
  StepEnd :: Step a -> TimePoint a
  deriving (Show, Typeable)

instance ConsIndex (TimePoint p) where
  consIndex = \case
    PhaseBegin{} -> 1
    PhaseEnd{} -> 2
    StepBegin{} -> 3
    StepEnd{} -> 4
