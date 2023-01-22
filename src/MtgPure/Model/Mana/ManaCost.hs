{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  ManaCost (..),
  emptyManaCost,
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (..))

data DynamicManaCost (var :: Var) = DynamicManaCost
  { costGeneric :: Mana var 'NonSnow 'MTGeneric
  , costSnow :: Mana var 'Snow 'MTGeneric
  , -- TODO: other hybrid costs
    costHybridBG :: Mana var 'NonSnow 'MTHybridBG
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

instance Semigroup (DynamicManaCost var) where
  dmc1 <> dmc2 =
    DynamicManaCost
      { costGeneric = g1 <> g2
      , costSnow = s1 <> s2
      , costHybridBG = bg1 <> bg2
      }
   where
    DynamicManaCost
      { costGeneric = g1
      , costSnow = s1
      , costHybridBG = bg1
      } = dmc1
    DynamicManaCost
      { costGeneric = g2
      , costSnow = s2
      , costHybridBG = bg2
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

instance Monoid (DynamicManaCost var) where
  mempty =
    DynamicManaCost
      { costGeneric = mempty
      , costSnow = mempty
      , costHybridBG = mempty
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
