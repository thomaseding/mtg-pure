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

module MtgPure.Model.Mana
  ( Mana (..),
  )
where

import safe Data.Kind (Type)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColoredMana (ColoredMana)
import safe MtgPure.Model.ColorlessMana (ColorlessMana)
import safe MtgPure.Model.GenericMana (GenericMana)
import safe MtgPure.Model.ManaType (ManaType (..))

data Mana :: ManaType -> Type where
  WhiteMana :: ColoredMana 'White -> Mana 'MTWhite
  BlueMana :: ColoredMana 'Blue -> Mana 'MTBlue
  BlackMana :: ColoredMana 'Black -> Mana 'MTBlack
  RedMana :: ColoredMana 'Red -> Mana 'MTRed
  GreenMana :: ColoredMana 'Green -> Mana 'MTGreen
  ColorlessMana :: ColorlessMana -> Mana 'MTColorless
  GenericMana :: GenericMana -> Mana 'MTGeneric

deriving instance Eq (Mana a)

deriving instance Ord (Mana a)

deriving instance Show (Mana a)

instance Semigroup (Mana 'MTWhite) where
  WhiteMana x <> WhiteMana y = WhiteMana (x <> y)

instance Semigroup (Mana 'MTBlue) where
  BlueMana x <> BlueMana y = BlueMana (x <> y)

instance Semigroup (Mana 'MTBlack) where
  BlackMana x <> BlackMana y = BlackMana (x <> y)

instance Semigroup (Mana 'MTRed) where
  RedMana x <> RedMana y = RedMana (x <> y)

instance Semigroup (Mana 'MTGreen) where
  GreenMana x <> GreenMana y = GreenMana (x <> y)

instance Semigroup (Mana 'MTColorless) where
  ColorlessMana x <> ColorlessMana y = ColorlessMana (x <> y)

instance Semigroup (Mana 'MTGeneric) where
  GenericMana x <> GenericMana y = GenericMana (x <> y)

instance Monoid (Mana 'MTWhite) where
  mempty = WhiteMana mempty

instance Monoid (Mana 'MTBlue) where
  mempty = BlueMana mempty

instance Monoid (Mana 'MTBlack) where
  mempty = BlackMana mempty

instance Monoid (Mana 'MTRed) where
  mempty = RedMana mempty

instance Monoid (Mana 'MTGreen) where
  mempty = GreenMana mempty

instance Monoid (Mana 'MTColorless) where
  mempty = ColorlessMana mempty

instance Monoid (Mana 'MTGeneric) where
  mempty = GenericMana mempty
