{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaPool (
  ManaPool (..),
  CompleteManaPool (..),
  ManaPayment (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.Mana (Mana)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (NoVar))

data ManaPool (snow :: Snow) = ManaPool
  { poolW :: Mana 'NoVar snow 'TyW
  , poolU :: Mana 'NoVar snow 'TyU
  , poolB :: Mana 'NoVar snow 'TyB
  , poolR :: Mana 'NoVar snow 'TyR
  , poolG :: Mana 'NoVar snow 'TyG
  , poolC :: Mana 'NoVar snow 'TyC
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data CompleteManaPool = CompleteManaPool
  { poolSnow :: ManaPool 'Snow
  , poolNonSnow :: ManaPool 'NonSnow
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data ManaPayment = ManaPayment
  { paymentMana :: CompleteManaPool
  , paymentLife :: Life -- For Phyrexian mana
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

instance Semigroup (ManaPool snow) where
  mp1 <> mp2 =
    ManaPool
      { poolW = w1 <> w2
      , poolU = u1 <> u2
      , poolB = b1 <> b2
      , poolR = r1 <> r2
      , poolG = g1 <> g2
      , poolC = c1 <> c2
      }
   where
    ManaPool
      { poolW = w1
      , poolU = u1
      , poolB = b1
      , poolR = r1
      , poolG = g1
      , poolC = c1
      } = mp1
    ManaPool
      { poolW = w2
      , poolU = u2
      , poolB = b2
      , poolR = r2
      , poolG = g2
      , poolC = c2
      } = mp2

instance Semigroup ManaPayment where
  mp1 <> mp2 =
    ManaPayment
      { paymentMana = m1 <> m2
      , paymentLife = Life $ l1 + l2
      }
   where
    ManaPayment
      { paymentMana = m1
      , paymentLife = Life l1
      } = mp1
    ManaPayment
      { paymentMana = m2
      , paymentLife = Life l2
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
      { poolW = mempty
      , poolU = mempty
      , poolB = mempty
      , poolR = mempty
      , poolG = mempty
      , poolC = mempty
      }

instance Monoid CompleteManaPool where
  mempty =
    CompleteManaPool
      { poolSnow = mempty
      , poolNonSnow = mempty
      }

instance Monoid ManaPayment where
  mempty =
    ManaPayment
      { paymentMana = mempty
      , paymentLife = Life 0
      }
