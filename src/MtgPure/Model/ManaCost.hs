{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaCost (
  ManaCost (..),
  emptyManaCost,
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana (IsManaNoVar, Mana (..), Snow (..))
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.Variable (ForceVars (..), Var (..))

data ManaCost (var :: Var) = ManaCost'
  { costWhite :: Mana var 'NonSnow 'MTWhite
  , costBlue :: Mana var 'NonSnow 'MTBlue
  , costBlack :: Mana var 'NonSnow 'MTBlack
  , costRed :: Mana var 'NonSnow 'MTRed
  , costGreen :: Mana var 'NonSnow 'MTGreen
  , costColorless :: Mana var 'NonSnow 'MTColorless
  , costGeneric :: Mana var 'NonSnow 'MTGeneric
  , costSnow :: Mana var 'Snow 'MTGeneric -- NOTE: I could support RealManaType snow costs if I want.
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
    }

instance Monoid (ManaCost var) where
  mempty = emptyManaCost

mapManaCost ::
  ( forall snow color.
    IsManaNoVar snow color =>
    Mana var snow color ->
    Mana var' snow color
  ) ->
  ManaCost var ->
  ManaCost var'
mapManaCost f (ManaCost' w u b r g c x s) =
  ManaCost' (f w) (f u) (f b) (f r) (f g) (f c) (f x) (f s)

mapManaCost2 ::
  ( forall snow color.
    IsManaNoVar snow color =>
    Mana var snow color ->
    Mana var snow color ->
    Mana var' snow color
  ) ->
  ManaCost var ->
  ManaCost var ->
  ManaCost var'
mapManaCost2
  f
  (ManaCost' w1 u1 b1 r1 g1 c1 x1 s1)
  (ManaCost' w2 u2 b2 r2 g2 c2 x2 s2) =
    ManaCost'
      (f w1 w2)
      (f u1 u2)
      (f b1 b2)
      (f r1 r2)
      (f g1 g2)
      (f c1 c2)
      (f x1 x2)
      (f s1 s2)

instance Num (ManaCost 'NoVar) where
  (+) = mapManaCost2 (+)
  (-) = mapManaCost2 (-)
  (*) = mapManaCost2 (*)
  abs = mapManaCost abs
  signum = mapManaCost signum
  negate = mapManaCost negate
  fromInteger x = ManaCost' 0 0 0 0 0 0 (fromInteger x) 0

instance ForceVars (ManaCost var) (ManaCost 'NoVar) where
  forceVars = mapManaCost forceVars
