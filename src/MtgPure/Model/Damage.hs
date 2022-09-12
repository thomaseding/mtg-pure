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

module MtgPure.Model.Damage
  ( Damage (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.ObjectN (OCreature)
import MtgPure.Model.Variable (Variable)

data Damage :: Type where
  Damage :: Int -> Damage
  DamageFromPower :: OCreature -> Damage
  VariableDamage :: Variable -> Damage
