{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CountMana (
  CountMana (..),
) where

import safe MtgPure.Model.ColoredMana (ColoredMana (..))
import safe MtgPure.Model.ColorlessMana (ColorlessMana (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.Mana (Mana (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Variable (Var (NoVar))

class CountMana a where
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
    WhiteMana mana -> countMana mana
    BlueMana mana -> countMana mana
    BlackMana mana -> countMana mana
    RedMana mana -> countMana mana
    GreenMana mana -> countMana mana
    ColorlessMana mana -> countMana mana
    GenericMana mana -> countMana mana

instance CountMana (ColoredMana 'NoVar mt) where
  countMana = \case
    ColoredMana' _sym n -> n

instance CountMana (ColorlessMana 'NoVar) where
  countMana = \case
    ColorlessMana' n -> n

instance CountMana (GenericMana 'NoVar) where
  countMana = \case
    GenericMana' n -> n
