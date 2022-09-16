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

module MtgPure.Model.AbilityType (
  AbilityType (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.EffectType (EffectType (..))

data AbilityType :: EffectType -> Type where
  ActivatedAbility :: AbilityType 'OneShot
  ManaAbility :: AbilityType 'OneShot
  StaticAbility :: AbilityType 'Continuous
  TriggeredAbility :: AbilityType 'OneShot
  deriving (Typeable)

deriving instance Eq (AbilityType e)

deriving instance Ord (AbilityType e)
