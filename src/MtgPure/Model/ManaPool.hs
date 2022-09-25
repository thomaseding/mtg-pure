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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaPool (
  ManaPool (..),
  CompleteManaPool (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana (IsManaNoVar, IsSnow, Mana, Snow (..))
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.Variable (Var (NoVar))

data ManaPool (snow :: Snow) = ManaPool
  { poolWhite :: Mana 'NoVar snow 'MTWhite
  , poolBlue :: Mana 'NoVar snow 'MTBlue
  , poolBlack :: Mana 'NoVar snow 'MTBlack
  , poolRed :: Mana 'NoVar snow 'MTRed
  , poolGreen :: Mana 'NoVar snow 'MTGreen
  , poolColorless :: Mana 'NoVar snow 'MTColorless
  }
  deriving (Eq, Ord, Show, Typeable) --  TODO: Make some of these orphans

data CompleteManaPool = CompleteManaPool
  { poolSnow :: ManaPool 'Snow
  , poolNonSnow :: ManaPool 'NonSnow
  }
  deriving (Show, Typeable)

instance Semigroup (ManaPool snow) where
  mp1 <> mp2 =
    ManaPool
      { poolWhite = w1 <> w2
      , poolBlue = u1 <> u2
      , poolBlack = b1 <> b2
      , poolRed = r1 <> r2
      , poolGreen = g1 <> g2
      , poolColorless = c1 <> c2
      }
   where
    ManaPool
      { poolWhite = w1
      , poolBlue = u1
      , poolBlack = b1
      , poolRed = r1
      , poolGreen = g1
      , poolColorless = c1
      } = mp1
    ManaPool
      { poolWhite = w2
      , poolBlue = u2
      , poolBlack = b2
      , poolRed = r2
      , poolGreen = g2
      , poolColorless = c2
      } = mp2

instance Semigroup CompleteManaPool where
  mp1 <> mp2 =
    CompleteManaPool
      { poolSnow = s1 <> s2
      , poolNonSnow = n1 <> n2
      }
   where
    CompleteManaPool
      { poolSnow = s1
      , poolNonSnow = n1
      } = mp1
    CompleteManaPool
      { poolSnow = s2
      , poolNonSnow = n2
      } = mp2

instance Monoid (ManaPool snow) where
  mempty =
    ManaPool
      { poolWhite = mempty
      , poolBlue = mempty
      , poolBlack = mempty
      , poolRed = mempty
      , poolGreen = mempty
      , poolColorless = mempty
      }

instance Monoid CompleteManaPool where
  mempty =
    CompleteManaPool
      { poolSnow = mempty
      , poolNonSnow = mempty
      }

mapManaPool ::
  IsSnow snow =>
  ( forall color.
    IsManaNoVar snow color =>
    Mana 'NoVar snow color ->
    Mana 'NoVar snow color
  ) ->
  ManaPool snow ->
  ManaPool snow
mapManaPool f (ManaPool w u b r g c) =
  ManaPool (f w) (f u) (f b) (f r) (f g) (f c)

mapManaPool2 ::
  IsSnow snow =>
  ( forall color.
    IsManaNoVar snow color =>
    Mana 'NoVar snow color ->
    Mana 'NoVar snow color ->
    Mana 'NoVar snow color
  ) ->
  ManaPool snow ->
  ManaPool snow ->
  ManaPool snow
mapManaPool2
  f
  (ManaPool w1 u1 b1 r1 g1 c1)
  (ManaPool w2 u2 b2 r2 g2 c2) =
    ManaPool
      (f w1 w2)
      (f u1 u2)
      (f b1 b2)
      (f r1 r2)
      (f g1 g2)
      (f c1 c2)

instance IsSnow snow => Num (ManaPool snow) where
  (+) = mapManaPool2 (+)
  (-) = mapManaPool2 (-)
  (*) = mapManaPool2 (*)
  abs = mapManaPool abs
  signum = mapManaPool signum
  negate = mapManaPool negate
  fromInteger = \case
    0 -> mempty
    -- NOTE: I could support a sound `fromInteger` definition, but it prolly isn't worth
    -- it because it would have to support only Colorless or all the mana types together,
    -- neither of which is particularly desirable.
    _ -> error "(fromInteger :: ManaPool snow) only supports n=0"

mapCompleteManaPool ::
  ( forall snow.
    IsSnow snow =>
    ManaPool snow ->
    ManaPool snow
  ) ->
  CompleteManaPool ->
  CompleteManaPool
mapCompleteManaPool f (CompleteManaPool snow nonSnow) =
  CompleteManaPool (f snow) (f nonSnow)

mapCompleteManaPool2 ::
  ( forall snow.
    IsSnow snow =>
    ManaPool snow ->
    ManaPool snow ->
    ManaPool snow
  ) ->
  CompleteManaPool ->
  CompleteManaPool ->
  CompleteManaPool
mapCompleteManaPool2
  f
  (CompleteManaPool snow1 nonSnow1)
  (CompleteManaPool snow2 nonSnow2) =
    CompleteManaPool
      (f snow1 snow2)
      (f nonSnow1 nonSnow2)

instance Num CompleteManaPool where
  (+) = mapCompleteManaPool2 (+)
  (-) = mapCompleteManaPool2 (-)
  (*) = mapCompleteManaPool2 (*)
  abs = mapCompleteManaPool abs
  signum = mapCompleteManaPool signum
  negate = mapCompleteManaPool negate
  fromInteger n = mempty{poolNonSnow = fromInteger n}
