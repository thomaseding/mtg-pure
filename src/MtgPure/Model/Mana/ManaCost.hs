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
  { hybridWU :: Mana var 'NonSnow 'MTHybridWU
  , hybridUB :: Mana var 'NonSnow 'MTHybridUB
  , hybridBR :: Mana var 'NonSnow 'MTHybridBR
  , hybridRG :: Mana var 'NonSnow 'MTHybridRG
  , hybridGW :: Mana var 'NonSnow 'MTHybridGW
  , hybridWB :: Mana var 'NonSnow 'MTHybridWB
  , hybridUR :: Mana var 'NonSnow 'MTHybridUR
  , hybridBG :: Mana var 'NonSnow 'MTHybridBG
  , hybridRW :: Mana var 'NonSnow 'MTHybridRW
  , hybridGU :: Mana var 'NonSnow 'MTHybridGU
  , hybridW2 :: Mana var 'NonSnow 'MTHybridW2
  , hybridU2 :: Mana var 'NonSnow 'MTHybridU2
  , hybridB2 :: Mana var 'NonSnow 'MTHybridB2
  , hybridR2 :: Mana var 'NonSnow 'MTHybridR2
  , hybridG2 :: Mana var 'NonSnow 'MTHybridG2
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data DynamicManaCost (var :: Var) = DynamicManaCost
  { costGeneric :: Mana var 'NonSnow 'MTGeneric
  , costSnow :: Mana var 'Snow 'MTSnow
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
      { hybridWU = wu1 <> wu2
      , hybridUB = ub1 <> ub2
      , hybridBR = br1 <> br2
      , hybridRG = rg1 <> rg2
      , hybridGW = gw1 <> gw2
      , hybridWB = wb1 <> wb2
      , hybridUR = ur1 <> ur2
      , hybridBG = bg1 <> bg2
      , hybridRW = rw1 <> rw2
      , hybridGU = gu1 <> gu2
      , hybridW2 = w21 <> w22
      , hybridU2 = u21 <> u22
      , hybridB2 = b21 <> b22
      , hybridR2 = r21 <> r22
      , hybridG2 = g21 <> g22
      }
   where
    HybridManaCost
      { hybridWU = wu1
      , hybridUB = ub1
      , hybridBR = br1
      , hybridRG = rg1
      , hybridGW = gw1
      , hybridWB = wb1
      , hybridUR = ur1
      , hybridBG = bg1
      , hybridRW = rw1
      , hybridGU = gu1
      , hybridW2 = w21
      , hybridU2 = u21
      , hybridB2 = b21
      , hybridR2 = r21
      , hybridG2 = g21
      } = hmc1
    HybridManaCost
      { hybridWU = wu2
      , hybridUB = ub2
      , hybridBR = br2
      , hybridRG = rg2
      , hybridGW = gw2
      , hybridWB = wb2
      , hybridUR = ur2
      , hybridBG = bg2
      , hybridRW = rw2
      , hybridGU = gu2
      , hybridW2 = w22
      , hybridU2 = u22
      , hybridB2 = b22
      , hybridR2 = r22
      , hybridG2 = g22
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
      { hybridWU = mempty
      , hybridUB = mempty
      , hybridBR = mempty
      , hybridRG = mempty
      , hybridGW = mempty
      , hybridWB = mempty
      , hybridUR = mempty
      , hybridBG = mempty
      , hybridRW = mempty
      , hybridGU = mempty
      , hybridW2 = mempty
      , hybridU2 = mempty
      , hybridB2 = mempty
      , hybridR2 = mempty
      , hybridG2 = mempty
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
