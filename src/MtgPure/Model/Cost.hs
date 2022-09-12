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

module MtgPure.Model.Cost
  ( Cost (..),
    ActivationCost,
    SpellCost,
  )
where

import Data.Kind (Type)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Loyalty (Loyalty)
import MtgPure.Model.ManaCost (ManaCost)
import MtgPure.Model.Object (OPlaneswalker, OPlayer, Object)
import MtgPure.Model.ObjectN (OPermanent)
import MtgPure.Model.Permanent (Permanent)
import MtgPure.Model.Requirement (Requirement)

data Cost :: Type where
  ManaCostCost :: ManaCost -> Cost
  AndCosts :: [Cost] -> Cost
  OrCosts :: [Cost] -> Cost
  TapCost :: OPermanent -> Cost
  LoyaltyCost :: OPlaneswalker -> Loyalty -> Cost
  DiscardRandomCost :: OPlayer -> Int -> Cost
  PayLife :: OPlayer -> Int -> Cost
  SacrificeCost :: IsObjectType a => Permanent a -> OPlayer -> [Requirement a] -> Cost

type ActivationCost a = Object a -> OPlayer -> Cost

type SpellCost = OPlayer -> Cost
