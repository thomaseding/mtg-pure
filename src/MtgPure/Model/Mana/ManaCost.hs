{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaCost (
  ManaCost (..),
  emptyManaCost,
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (..))

data ManaCost (var :: Var) = ManaCost'
  { costWhite :: Mana var 'NonSnow 'MTWhite
  , costBlue :: Mana var 'NonSnow 'MTBlue
  , costBlack :: Mana var 'NonSnow 'MTBlack
  , costRed :: Mana var 'NonSnow 'MTRed
  , costGreen :: Mana var 'NonSnow 'MTGreen
  , costColorless :: Mana var 'NonSnow 'MTColorless
  , costGeneric :: Mana var 'NonSnow 'MTGeneric
  , costSnow :: Mana var 'Snow 'MTGeneric
  , -- TODO: other hybrid costs
    costHybridBG :: Mana var 'NonSnow 'MTHybridBG
  }
  deriving (Eq, Ord, Typeable) --  TODO: Make some of these orphans

instance Semigroup (ManaCost var) where
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
      , costHybridBG = bg1 <> bg2
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
      , costHybridBG = bg1
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
      , costHybridBG = bg2
      } = mc2

emptyManaCost :: ManaCost var
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
    , costHybridBG = mempty
    }

instance Monoid (ManaCost var) where
  mempty = emptyManaCost
