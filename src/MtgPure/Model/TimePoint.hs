{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.TimePoint
  ( TimePoint (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.Phase (Phase, SPhase)
import MtgPure.Model.Step (Step)

data TimePoint (a :: Phase) :: Type where
  PhaseBegin :: SPhase a -> TimePoint a
  PhaseEnd :: SPhase a -> TimePoint a
  StepBegin :: Step a -> TimePoint a
  StepEnd :: Step a -> TimePoint a
  deriving (Show)