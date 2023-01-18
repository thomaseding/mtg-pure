{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaPool (
  ManaPool (..),
  CompleteManaPool (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Mana (Mana)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (NoVar))

data ManaPool (snow :: Snow) = ManaPool
  { poolWhite :: Mana 'NoVar snow 'MTWhite
  , poolBlue :: Mana 'NoVar snow 'MTBlue
  , poolBlack :: Mana 'NoVar snow 'MTBlack
  , poolRed :: Mana 'NoVar snow 'MTRed
  , poolGreen :: Mana 'NoVar snow 'MTGreen
  , poolColorless :: Mana 'NoVar snow 'MTColorless
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data CompleteManaPool = CompleteManaPool
  { poolSnow :: ManaPool 'Snow
  , poolNonSnow :: ManaPool 'NonSnow
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

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
