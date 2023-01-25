{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.CountMana (
  CountMana (..),
) where

import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..), HybridManaCost (..), PhyrexianManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Variable (Var (NoVar))

class CountMana a where
  -- Hybrid and phyrexian mana are counted as 1 mana.
  countMana :: a -> Int

instance CountMana CompleteManaPool where
  countMana
    CompleteManaPool
      { poolSnow = p0
      , poolNonSnow = p1
      } = countMana p0 + countMana p1

instance CountMana (ManaPool snow) where
  countMana
    ManaPool
      { poolWhite = w
      , poolBlue = u
      , poolBlack = b
      , poolRed = r
      , poolGreen = g
      , poolColorless = c
      } = countMana w + countMana u + countMana b + countMana r + countMana g + countMana c

instance CountMana (Mana 'NoVar snow mt) where
  countMana = \case
    Mana mana -> mana

instance CountMana (PhyrexianManaCost 'NoVar) where
  countMana
    PhyrexianManaCost
      { phyrexianW = w
      , phyrexianU = u
      , phyrexianB = b
      , phyrexianR = r
      , phyrexianG = g
      } = countMana w + countMana u + countMana b + countMana r + countMana g

instance CountMana (HybridManaCost 'NoVar) where
  countMana
    HybridManaCost
      { hybridBG = bg
      } = countMana bg

instance CountMana (DynamicManaCost 'NoVar) where
  countMana
    DynamicManaCost
      { costGeneric = g
      , costSnow = s
      , costHybrid = h
      , costPhyrexian = p
      } =
      countMana g + countMana s + countMana h + countMana p
