{-# LANGUAGE Safe #-}
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
  { phyrexianW :: Mana var 'NonSnow 'TyPW
  , phyrexianU :: Mana var 'NonSnow 'TyPU
  , phyrexianB :: Mana var 'NonSnow 'TyPB
  , phyrexianR :: Mana var 'NonSnow 'TyPR
  , phyrexianG :: Mana var 'NonSnow 'TyPG
  , phyrexianC :: Mana var 'NonSnow 'TyPC
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

-- TODO: other hybrid costs
data HybridManaCost (var :: Var) = HybridManaCost
  { hybridWU :: Mana var 'NonSnow 'TyWU
  , hybridUB :: Mana var 'NonSnow 'TyUB
  , hybridBR :: Mana var 'NonSnow 'TyBR
  , hybridRG :: Mana var 'NonSnow 'TyRG
  , hybridGW :: Mana var 'NonSnow 'TyGW
  , hybridWB :: Mana var 'NonSnow 'TyWB
  , hybridUR :: Mana var 'NonSnow 'TyUR
  , hybridBG :: Mana var 'NonSnow 'TyBG
  , hybridRW :: Mana var 'NonSnow 'TyRW
  , hybridGU :: Mana var 'NonSnow 'TyGU
  , hybridW2 :: Mana var 'NonSnow 'TyW2
  , hybridU2 :: Mana var 'NonSnow 'TyU2
  , hybridB2 :: Mana var 'NonSnow 'TyB2
  , hybridR2 :: Mana var 'NonSnow 'TyR2
  , hybridG2 :: Mana var 'NonSnow 'TyG2
  , hybridC2 :: Mana var 'NonSnow 'TyC2
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data DynamicManaCost (var :: Var) = DynamicManaCost
  { costGeneric :: Mana var 'NonSnow 'Ty1
  , costSnow :: Mana var 'Snow 'Ty1
  , costHybrid :: HybridManaCost var
  , costPhyrexian :: PhyrexianManaCost var
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

data ManaCost (var :: Var) = ManaCost'
  { costW :: Mana var 'NonSnow 'TyW
  , costU :: Mana var 'NonSnow 'TyU
  , costB :: Mana var 'NonSnow 'TyB
  , costR :: Mana var 'NonSnow 'TyR
  , costG :: Mana var 'NonSnow 'TyG
  , costC :: Mana var 'NonSnow 'TyC
  , costDynamic :: DynamicManaCost var
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

instance Semigroup (PhyrexianManaCost var) where
  pmc1 <> pmc2 =
    PhyrexianManaCost
      { phyrexianW = w1 <> w2
      , phyrexianU = u1 <> u2
      , phyrexianB = b1 <> b2
      , phyrexianR = r1 <> r2
      , phyrexianG = g1 <> g2
      , phyrexianC = c1 <> c2
      }
   where
    PhyrexianManaCost
      { phyrexianW = w1
      , phyrexianU = u1
      , phyrexianB = b1
      , phyrexianR = r1
      , phyrexianG = g1
      , phyrexianC = c1
      } = pmc1
    PhyrexianManaCost
      { phyrexianW = w2
      , phyrexianU = u2
      , phyrexianB = b2
      , phyrexianR = r2
      , phyrexianG = g2
      , phyrexianC = c2
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
      , hybridC2 = c21 <> c22
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
      , hybridC2 = c21
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
      , hybridC2 = c22
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
      { costW = w1 <> w2
      , costU = u1 <> u2
      , costB = b1 <> b2
      , costR = r1 <> r2
      , costG = g1 <> g2
      , costC = c1 <> c2
      , costDynamic = d1 <> d2
      }
   where
    ManaCost'
      { costW = w1
      , costU = u1
      , costB = b1
      , costR = r1
      , costG = g1
      , costC = c1
      , costDynamic = d1
      } = mc1
    ManaCost'
      { costW = w2
      , costU = u2
      , costB = b2
      , costR = r2
      , costG = g2
      , costC = c2
      , costDynamic = d2
      } = mc2

instance Monoid (PhyrexianManaCost var) where
  mempty =
    PhyrexianManaCost
      { phyrexianW = mempty
      , phyrexianU = mempty
      , phyrexianB = mempty
      , phyrexianR = mempty
      , phyrexianG = mempty
      , phyrexianC = mempty
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
      , hybridC2 = mempty
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
      { costW = mempty
      , costU = mempty
      , costB = mempty
      , costR = mempty
      , costG = mempty
      , costC = mempty
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
