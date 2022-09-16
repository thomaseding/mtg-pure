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

module MtgPure.Model.ColoredMana (
  ColoredMana (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorToManaType (ColorToManaType)
import safe MtgPure.Model.ManaSymbol (ManaSymbol)
import safe MtgPure.Model.Variable (Variable)

data ColoredMana :: Color -> Type where
  ColoredMana' :: ManaSymbol (ColorToManaType c) -> Int -> ColoredMana c
  VariableColoredMana :: ManaSymbol (ColorToManaType c) -> Variable -> ColoredMana c
  SumColoredMana :: ManaSymbol (ColorToManaType c) -> ColoredMana c -> ColoredMana c -> ColoredMana c
  deriving (Typeable)

deriving instance Eq (ColoredMana c)

deriving instance Ord (ColoredMana c)

deriving instance Show (ColoredMana c)

instance Semigroup (ColoredMana 'White) where
  (<>) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' mempty (x + y)
  (<>) (ColoredMana' _ 0) y = y
  (<>) x (ColoredMana' _ 0) = x
  (<>) x y = SumColoredMana mempty x y

instance Semigroup (ColoredMana 'Blue) where
  (<>) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' mempty (x + y)
  (<>) (ColoredMana' _ 0) y = y
  (<>) x (ColoredMana' _ 0) = x
  (<>) x y = SumColoredMana mempty x y

instance Semigroup (ColoredMana 'Black) where
  (<>) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' mempty (x + y)
  (<>) (ColoredMana' _ 0) y = y
  (<>) x (ColoredMana' _ 0) = x
  (<>) x y = SumColoredMana mempty x y

instance Semigroup (ColoredMana 'Red) where
  (<>) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' mempty (x + y)
  (<>) (ColoredMana' _ 0) y = y
  (<>) x (ColoredMana' _ 0) = x
  (<>) x y = SumColoredMana mempty x y

instance Semigroup (ColoredMana 'Green) where
  (<>) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' mempty (x + y)
  (<>) (ColoredMana' _ 0) y = y
  (<>) x (ColoredMana' _ 0) = x
  (<>) x y = SumColoredMana mempty x y

instance Monoid (ColoredMana 'White) where
  mempty = ColoredMana' mempty 0

instance Monoid (ColoredMana 'Blue) where
  mempty = ColoredMana' mempty 0

instance Monoid (ColoredMana 'Black) where
  mempty = ColoredMana' mempty 0

instance Monoid (ColoredMana 'Red) where
  mempty = ColoredMana' mempty 0

instance Monoid (ColoredMana 'Green) where
  mempty = ColoredMana' mempty 0
