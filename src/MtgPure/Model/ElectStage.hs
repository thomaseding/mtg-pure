{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ElectStage (
  ElectStage (..),
  SElectStage (..),
  IsElectStage (..),
  NonIntrinsicStage (..),
  CoNonIntrinsicStage (..),
  ElectStageRW,
) where

import safe Control.Monad.Access (ReadWrite (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data ElectStage :: Type where
  IntrinsicStage :: ElectStage
  TargetStage :: ElectStage
  ResolveStage :: ElectStage
  deriving (Typeable)

data SElectStage :: ElectStage -> Type where
  SIntrinsicStage :: SElectStage 'IntrinsicStage
  STargetStage :: SElectStage 'TargetStage
  SResolveStage :: SElectStage 'ResolveStage

class IsElectStage (s :: ElectStage) where
  singElectStage :: SElectStage s

instance IsElectStage 'IntrinsicStage where
  singElectStage = SIntrinsicStage

instance IsElectStage 'TargetStage where
  singElectStage = STargetStage

instance IsElectStage 'ResolveStage where
  singElectStage = SResolveStage

data NonIntrinsicStage (s :: ElectStage) :: Type where
  NonIntrinsicTargetStage :: NonIntrinsicStage 'TargetStage
  NonIntrinsicResolveStage :: NonIntrinsicStage 'ResolveStage

class IsElectStage s => CoNonIntrinsicStage (s :: ElectStage) where
  coNonIntrinsicStage :: NonIntrinsicStage s

instance CoNonIntrinsicStage 'TargetStage where
  coNonIntrinsicStage = NonIntrinsicTargetStage

instance CoNonIntrinsicStage 'ResolveStage where
  coNonIntrinsicStage = NonIntrinsicResolveStage

type family ElectStageRW (s :: ElectStage) = (rw :: ReadWrite) where
  ElectStageRW 'IntrinsicStage = 'RO
  ElectStageRW 'TargetStage = 'RW
  ElectStageRW 'ResolveStage = 'RW
