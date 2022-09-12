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

module MtgPure.Model.Requirement
  ( Requirement (..),
    RequirementN (..),
  )
where

import Data.Inst (Inst2, Inst3)
import Data.Kind (Type)
import MtgPure.Model.Colors (Colors)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Object (OPlayer, Object)
import MtgPure.Model.ObjectType (ObjectType)
import MtgPure.Model.Permanent (Permanent)

data Requirement :: ObjectType -> Type where
  Impossible :: IsObjectType a => Requirement a
  OfColors :: IsObjectType a => Colors -> Requirement a
  Not :: IsObjectType a => Requirement a -> Requirement a
  Is :: IsObjectType a => Object a -> Requirement a
  ControlledBy :: IsObjectType a => OPlayer -> Requirement a
  OwnedBy :: IsObjectType a => OPlayer -> Requirement a
  Tapped :: IsObjectType a => Permanent a -> Requirement a

data RequirementN :: forall a. a -> Type where
  Req2 :: Inst2 IsObjectType a b => [Requirement a] -> [Requirement b] -> RequirementN '(a, b)
  Req3 :: Inst3 IsObjectType a b c => [Requirement a] -> [Requirement b] -> [Requirement c] -> RequirementN '(a, b, c)
