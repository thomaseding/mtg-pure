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

module MtgPure.Model.ManaCost (
  ManaCost (..),
  emptyManaCost,
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana (Mana (..), Snow (..))
import safe MtgPure.Model.ManaType (ManaType (..))

data ManaCost = ManaCost'
  { costWhite :: Mana 'NonSnow 'MTWhite
  , costBlue :: Mana 'NonSnow 'MTBlue
  , costBlack :: Mana 'NonSnow 'MTBlack
  , costRed :: Mana 'NonSnow 'MTRed
  , costGreen :: Mana 'NonSnow 'MTGreen
  , costColorless :: Mana 'NonSnow 'MTColorless
  , costGeneric :: Mana 'NonSnow 'MTGeneric
  , costSnow :: Mana 'Snow 'MTGeneric -- NB: I could support RealManaType snow costs if I want.
  }
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup ManaCost where
  mc1 <> mc2 =
    ManaCost'
      { costWhite = w1 <> w2
      , costBlue = u1 <> u2
      , costBlack = b1 <> b2
      , costRed = r1 <> r2
      , costGreen = g1 <> g2
      , costColorless = c1 <> c2
      , costGeneric = x1 <> x2
      , costSnow = s1 <> s2
      }
   where
    ManaCost'
      { costWhite = w1
      , costBlue = u1
      , costBlack = b1
      , costRed = r1
      , costGreen = g1
      , costColorless = c1
      , costGeneric = x1
      , costSnow = s1
      } = mc1
    ManaCost'
      { costWhite = w2
      , costBlue = u2
      , costBlack = b2
      , costRed = r2
      , costGreen = g2
      , costColorless = c2
      , costGeneric = x2
      , costSnow = s2
      } = mc2

emptyManaCost :: ManaCost
emptyManaCost =
  ManaCost'
    { costWhite = mempty
    , costBlue = mempty
    , costBlack = mempty
    , costRed = mempty
    , costGreen = mempty
    , costColorless = mempty
    , costGeneric = mempty
    , costSnow = mempty
    }

instance Monoid ManaCost where
  mempty = emptyManaCost
