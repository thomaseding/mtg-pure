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

module MtgPure.Model.Mana (
  Mana (..),
  Snow (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColoredMana (ColoredMana)
import safe MtgPure.Model.ColorlessMana (ColorlessMana)
import safe MtgPure.Model.GenericMana (GenericMana)
import safe MtgPure.Model.ManaType (ManaType (..))

data Snow = Snow | NonSnow
  deriving (Typeable)

data Mana (snow :: Snow) (mt :: ManaType) :: Type where
  WhiteMana :: ColoredMana 'White -> Mana snow 'MTWhite
  BlueMana :: ColoredMana 'Blue -> Mana snow 'MTBlue
  BlackMana :: ColoredMana 'Black -> Mana snow 'MTBlack
  RedMana :: ColoredMana 'Red -> Mana snow 'MTRed
  GreenMana :: ColoredMana 'Green -> Mana snow 'MTGreen
  ColorlessMana :: ColorlessMana -> Mana snow 'MTColorless
  GenericMana :: GenericMana -> Mana snow 'MTGeneric
  deriving (Typeable)

deriving instance Eq (Mana snow mt)

deriving instance Ord (Mana snow mt)

deriving instance Show (Mana snow mt)

instance Semigroup (Mana snow 'MTWhite) where
  WhiteMana x <> WhiteMana y = WhiteMana (x <> y)

instance Semigroup (Mana snow 'MTBlue) where
  BlueMana x <> BlueMana y = BlueMana (x <> y)

instance Semigroup (Mana snow 'MTBlack) where
  BlackMana x <> BlackMana y = BlackMana (x <> y)

instance Semigroup (Mana snow 'MTRed) where
  RedMana x <> RedMana y = RedMana (x <> y)

instance Semigroup (Mana snow 'MTGreen) where
  GreenMana x <> GreenMana y = GreenMana (x <> y)

instance Semigroup (Mana snow 'MTColorless) where
  ColorlessMana x <> ColorlessMana y = ColorlessMana (x <> y)

instance Semigroup (Mana snow 'MTGeneric) where
  GenericMana x <> GenericMana y = GenericMana (x <> y)

instance Monoid (Mana snow 'MTWhite) where
  mempty = WhiteMana mempty

instance Monoid (Mana snow 'MTBlue) where
  mempty = BlueMana mempty

instance Monoid (Mana snow 'MTBlack) where
  mempty = BlackMana mempty

instance Monoid (Mana snow 'MTRed) where
  mempty = RedMana mempty

instance Monoid (Mana snow 'MTGreen) where
  mempty = GreenMana mempty

instance Monoid (Mana snow 'MTColorless) where
  mempty = ColorlessMana mempty

instance Monoid (Mana snow 'MTGeneric) where
  mempty = GenericMana mempty
