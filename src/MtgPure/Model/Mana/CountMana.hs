{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.CountMana (
  CountMana (..),
) where

import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (..),
  PhyrexianManaCost (..),
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPayment (..), ManaPool (..))
import safe MtgPure.Model.Variable (Var (NoVar))

class CountMana a where
  -- returns min mana count
  countMana :: a -> Int

instance CountMana CompleteManaPool where
  countMana
    CompleteManaPool
      { poolSnow = p0
      , poolNonSnow = p1
      } = countMana p0 + countMana p1

instance CountMana (ManaPool snow) where
  countMana (ManaPool w u b r g c) =
    countMana w + countMana u + countMana b + countMana r + countMana g + countMana c

instance CountMana ManaPayment where
  countMana (ManaPayment pool _life) = countMana pool

instance CountMana (Mana 'NoVar snow mt) where
  countMana = \case
    Mana mana -> mana

instance CountMana (PhyrexianManaCost 'NoVar) where
  countMana PhyrexianManaCost{} = 0

instance CountMana (HybridManaCost 'NoVar) where
  countMana (HybridManaCost wu ub br rg gw wb ur bg rw gu w2 u2 b2 r2 g2 c2) =
    countMana wu
      + countMana ub
      + countMana br
      + countMana rg
      + countMana gw
      + countMana wb
      + countMana ur
      + countMana bg
      + countMana rw
      + countMana gu
      + countMana w2
      + countMana u2
      + countMana b2
      + countMana r2
      + countMana g2
      + countMana c2

instance CountMana (DynamicManaCost 'NoVar) where
  countMana
    DynamicManaCost
      { costGeneric = g
      , costSnow = s
      , costHybrid = h
      , costPhyrexian = p
      } =
      countMana g + countMana s + countMana h + countMana p
