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
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Step
  ( Step (..),
  )
where

import safe Data.Kind (Type)
import safe MtgPure.Model.Phase (Phase (..))

data Step :: Phase -> Type where
  UntapStep :: Step 'BeginningPhase
  UpkeepStep :: Step 'BeginningPhase
  DrawStep :: Step 'BeginningPhase
  EndStep :: Step 'EndingPhase
  CleanupStep :: Step 'EndingPhase

deriving instance Eq (Step p)

deriving instance Show (Step p)
