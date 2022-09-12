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

module MtgPure.Model.Phase
  ( Phase (..),
    SPhase (..),
  )
where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Phase :: Type where
  BeginningPhase :: Phase
  PreCombatMainPhase :: Phase
  CombatPhase :: Phase
  PostCombatMainPhase :: Phase
  EndingPhase :: Phase
  deriving (Show, Typeable)

data SPhase :: Phase -> Type where
  SBeginningPhase :: SPhase 'BeginningPhase
  SPreCombatMainPhase :: SPhase 'PreCombatMainPhase
  SCombatPhase :: SPhase 'CombatPhase
  SPostCombatMainPhase :: SPhase 'PostCombatMainPhase
  SEndingPhase :: SPhase 'EndingPhase
  deriving (Typeable)

deriving instance Show (SPhase a)
