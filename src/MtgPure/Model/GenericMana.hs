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

module MtgPure.Model.GenericMana
  ( GenericMana (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.Variable (Variable)

data GenericMana :: Type where
  GenericMana' :: Int -> GenericMana
  VariableGenericMana :: Variable -> GenericMana
  SumGenericMana :: GenericMana -> GenericMana -> GenericMana
  deriving (Show)

deriving instance Eq GenericMana

instance Semigroup GenericMana where
  (<>) (GenericMana' x) (GenericMana' y) = GenericMana' (x + y)
  (<>) (GenericMana' 0) y = y
  (<>) x (GenericMana' 0) = x
  (<>) x y = SumGenericMana x y

instance Monoid GenericMana where
  mempty = GenericMana' 0
