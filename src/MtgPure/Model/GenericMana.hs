{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.GenericMana (
  GenericMana (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Variable (ForceVars (..), Var (..), Variable (..))

data GenericMana (v :: Var) :: Type where
  GenericMana' :: Int -> GenericMana v
  VariableGenericMana :: Variable Int -> GenericMana 'Var
  SumGenericMana :: GenericMana 'Var -> GenericMana 'Var -> GenericMana 'Var
  deriving (Typeable) --  TODO: Make some of these orphans

deriving instance Eq (GenericMana v)

deriving instance Ord (GenericMana v)

deriving instance Show (GenericMana v)

instance Semigroup (GenericMana v) where
  (<>) x y = case (x, y) of
    (GenericMana' a, GenericMana' b) -> GenericMana' (a + b)
    (GenericMana' 0, _) -> y
    (_, GenericMana' 0) -> x
    (VariableGenericMana{}, _) -> SumGenericMana x y
    (_, VariableGenericMana{}) -> SumGenericMana x y
    (SumGenericMana{}, _) -> SumGenericMana x y
    (_, SumGenericMana{}) -> SumGenericMana x y

instance Monoid (GenericMana v) where
  mempty = GenericMana' 0

instance Num (GenericMana 'NoVar) where
  (+) (GenericMana' x) (GenericMana' y) = GenericMana' $ x + y
  (-) (GenericMana' x) (GenericMana' y) = GenericMana' $ x - y
  (*) (GenericMana' x) (GenericMana' y) = GenericMana' $ x * y
  abs (GenericMana' x) = GenericMana' $ abs x
  signum (GenericMana' x) = GenericMana' $ signum x
  negate (GenericMana' x) = GenericMana' $ negate x
  fromInteger = GenericMana' . fromInteger

instance ForceVars (GenericMana v) (GenericMana 'NoVar) where
  forceVars = \case
    GenericMana' n -> GenericMana' n
    VariableGenericMana (ReifiedVariable _ n) -> GenericMana' n
    SumGenericMana x y -> forceVars x + forceVars y
