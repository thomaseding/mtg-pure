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

module MtgPure.Model.ManaPool
  ( ManaPool (..),
    emptyManaPool,
  )
where

import safe MtgPure.Model.Mana (Mana)
import safe MtgPure.Model.ManaType (ManaType (..))

data ManaPool = ManaPool
  { poolWhite :: Mana 'MTWhite,
    poolBlue :: Mana 'MTBlue,
    poolBlack :: Mana 'MTBlack,
    poolRed :: Mana 'MTRed,
    poolGreen :: Mana 'MTGreen,
    poolColorless :: Mana 'MTColorless
  }
  deriving (Eq, Ord, Show)

instance Semigroup ManaPool where
  mp1 <> mp2 =
    ManaPool
      { poolWhite = w1 <> w2,
        poolBlue = u1 <> u2,
        poolBlack = b1 <> b2,
        poolRed = r1 <> r2,
        poolGreen = g1 <> g2,
        poolColorless = c1 <> c2
      }
    where
      ManaPool
        { poolWhite = w1,
          poolBlue = u1,
          poolBlack = b1,
          poolRed = r1,
          poolGreen = g1,
          poolColorless = c1
        } = mp1
      ManaPool
        { poolWhite = w2,
          poolBlue = u2,
          poolBlack = b2,
          poolRed = r2,
          poolGreen = g2,
          poolColorless = c2
        } = mp2

emptyManaPool :: ManaPool
emptyManaPool =
  ManaPool
    { poolWhite = mempty,
      poolBlue = mempty,
      poolBlack = mempty,
      poolRed = mempty,
      poolGreen = mempty,
      poolColorless = mempty
    }

instance Monoid ManaPool where
  mempty = emptyManaPool
