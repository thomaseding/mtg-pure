{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module MtgPure.Model.Mana.ManaCost (
  PhyrexianManaCost (..),
  HybridManaCost (..),
  DynamicManaCost (..),
  ManaCost (..),
  emptyManaCost,
  isOnlyGeneric,
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (..))

data PhyrexianManaCost (var :: Var) = PhyrexianManaCost
  { phyrexianWhite :: Mana var 'NonSnow 'MTPhyrexianWhite
  , phyrexianBlue :: Mana var 'NonSnow 'MTPhyrexianBlue
  , phyrexianBlack :: Mana var 'NonSnow 'MTPhyrexianBlack
  , phyrexianRed :: Mana var 'NonSnow 'MTPhyrexianRed
  , phyrexianGreen :: Mana var 'NonSnow 'MTPhyrexianGreen
  , phyrexianColorless :: Mana var 'NonSnow 'MTPhyrexianColorless
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

-- TODO: other hybrid costs
data HybridManaCost (var :: Var) = HybridManaCost
  { hybridBG :: Mana var 'NonSnow 'MTHybridBG
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data DynamicManaCost (var :: Var) = DynamicManaCost
  { costGeneric :: Mana var 'NonSnow 'MTGeneric
  , costSnow :: Mana var 'Snow 'MTGeneric
  , costHybrid :: HybridManaCost var
  , costPhyrexian :: PhyrexianManaCost var
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data ManaCost (var :: Var) = ManaCost'
  { costWhite :: Mana var 'NonSnow 'MTWhite
  , costBlue :: Mana var 'NonSnow 'MTBlue
  , costBlack :: Mana var 'NonSnow 'MTBlack
  , costRed :: Mana var 'NonSnow 'MTRed
  , costGreen :: Mana var 'NonSnow 'MTGreen
  , costColorless :: Mana var 'NonSnow 'MTColorless
  , costDynamic :: DynamicManaCost var
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

instance Semigroup (PhyrexianManaCost var) where
  pmc1 <> pmc2 =
    PhyrexianManaCost
      { phyrexianWhite = w1 <> w2
      , phyrexianBlue = u1 <> u2
      , phyrexianBlack = b1 <> b2
      , phyrexianRed = r1 <> r2
      , phyrexianGreen = g1 <> g2
      , phyrexianColorless = c1 <> c2
      }
   where
    PhyrexianManaCost
      { phyrexianWhite = w1
      , phyrexianBlue = u1
      , phyrexianBlack = b1
      , phyrexianRed = r1
      , phyrexianGreen = g1
      , phyrexianColorless = c1
      } = pmc1
    PhyrexianManaCost
      { phyrexianWhite = w2
      , phyrexianBlue = u2
      , phyrexianBlack = b2
      , phyrexianRed = r2
      , phyrexianGreen = g2
      , phyrexianColorless = c2
      } = pmc2

instance Semigroup (HybridManaCost var) where
  hmc1 <> hmc2 =
    HybridManaCost
      { hybridBG = bg1 <> bg2
      }
   where
    HybridManaCost
      { hybridBG = bg1
      } = hmc1
    HybridManaCost
      { hybridBG = bg2
      } = hmc2

instance Semigroup (DynamicManaCost var) where
  dmc1 <> dmc2 =
    DynamicManaCost
      { costGeneric = g1 <> g2
      , costSnow = s1 <> s2
      , costHybrid = h1 <> h2
      , costPhyrexian = p1 <> p2
      }
   where
    DynamicManaCost
      { costGeneric = g1
      , costSnow = s1
      , costHybrid = h1
      , costPhyrexian = p1
      } = dmc1
    DynamicManaCost
      { costGeneric = g2
      , costSnow = s2
      , costHybrid = h2
      , costPhyrexian = p2
      } = dmc2

instance Semigroup (ManaCost var) where
  mc1 <> mc2 =
    ManaCost'
      { costWhite = w1 <> w2
      , costBlue = u1 <> u2
      , costBlack = b1 <> b2
      , costRed = r1 <> r2
      , costGreen = g1 <> g2
      , costColorless = c1 <> c2
      , costDynamic = d1 <> d2
      }
   where
    ManaCost'
      { costWhite = w1
      , costBlue = u1
      , costBlack = b1
      , costRed = r1
      , costGreen = g1
      , costColorless = c1
      , costDynamic = d1
      } = mc1
    ManaCost'
      { costWhite = w2
      , costBlue = u2
      , costBlack = b2
      , costRed = r2
      , costGreen = g2
      , costColorless = c2
      , costDynamic = d2
      } = mc2

instance Monoid (PhyrexianManaCost var) where
  mempty =
    PhyrexianManaCost
      { phyrexianWhite = mempty
      , phyrexianBlue = mempty
      , phyrexianBlack = mempty
      , phyrexianRed = mempty
      , phyrexianGreen = mempty
      , phyrexianColorless = mempty
      }

instance Monoid (HybridManaCost var) where
  mempty =
    HybridManaCost
      { hybridBG = mempty
      }

instance Monoid (DynamicManaCost var) where
  mempty =
    DynamicManaCost
      { costGeneric = mempty
      , costSnow = mempty
      , costHybrid = mempty
      , costPhyrexian = mempty
      }

instance Monoid (ManaCost var) where
  mempty =
    ManaCost'
      { costWhite = mempty
      , costBlue = mempty
      , costBlack = mempty
      , costRed = mempty
      , costGreen = mempty
      , costColorless = mempty
      , costDynamic = mempty
      }

emptyManaCost :: ManaCost var
emptyManaCost = mempty

isOnlyGeneric :: DynamicManaCost var -> Bool
isOnlyGeneric
  DynamicManaCost
    { costGeneric = _x
    , costSnow = s
    , costHybrid = hy
    , costPhyrexian = phy
    } = s == mempty && hy == mempty && phy == mempty
