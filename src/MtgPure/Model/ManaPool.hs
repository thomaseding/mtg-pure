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

module MtgPure.Model.ManaPool (
  ManaPool (..),
  CompleteManaPool (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana (Mana, Snow (..))
import safe MtgPure.Model.ManaType (ManaType (..))

data ManaPool (snow :: Snow) = ManaPool
  { poolWhite :: Mana snow 'MTWhite
  , poolBlue :: Mana snow 'MTBlue
  , poolBlack :: Mana snow 'MTBlack
  , poolRed :: Mana snow 'MTRed
  , poolGreen :: Mana snow 'MTGreen
  , poolColorless :: Mana snow 'MTColorless
  }
  deriving (Eq, Ord, Show, Typeable)

data CompleteManaPool = CompleteManaPool
  { poolSnow :: ManaPool 'Snow
  , poolNonSnow :: ManaPool 'NonSnow
  }
  deriving (Eq, Ord, Show, Typeable)

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
