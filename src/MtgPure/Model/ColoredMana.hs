{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColoredMana (
  ColorToManaSymbol,
  colorToManaSymbol,
  ColoredMana (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColorToManaType (ColorToManaType)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Variable (ForceVars (..), Var (..), Variable (..))

class ColorToManaSymbol' (color :: Color) where
  colorToManaSymbol :: ManaSymbol (ColorToManaType color)

type ColorToManaSymbol = ColorToManaSymbol'

instance ColorToManaSymbol' 'White where
  colorToManaSymbol = W

instance ColorToManaSymbol' 'Blue where
  colorToManaSymbol = U

instance ColorToManaSymbol' 'Black where
  colorToManaSymbol = B

instance ColorToManaSymbol' 'Red where
  colorToManaSymbol = R

instance ColorToManaSymbol' 'Green where
  colorToManaSymbol = G

data ColoredMana (var :: Var) (color :: Color) :: Type where
  ColoredMana' :: forall v c. ManaSymbol (ColorToManaType c) -> Int -> ColoredMana v c
  VariableColoredMana :: ManaSymbol (ColorToManaType c) -> Variable Int -> ColoredMana 'Var c
  SumColoredMana :: ManaSymbol (ColorToManaType c) -> ColoredMana 'Var c -> ColoredMana 'Var c -> ColoredMana 'Var c
  deriving (Typeable) --  TODO: Make some of these orphans

deriving instance Eq (ColoredMana v c)

deriving instance Ord (ColoredMana v c)

deriving instance Show (ColoredMana v c)

instance ColorToManaSymbol color => Semigroup (ColoredMana v color) where
  (<>) x y = case (x, y) of
    (ColoredMana' _ a, ColoredMana' _ b) -> ColoredMana' colorToManaSymbol (a + b)
    (ColoredMana' _ 0, _) -> y
    (_, ColoredMana' _ 0) -> x
    (VariableColoredMana{}, _) -> SumColoredMana colorToManaSymbol x y
    (_, VariableColoredMana{}) -> SumColoredMana colorToManaSymbol x y
    (SumColoredMana{}, _) -> SumColoredMana colorToManaSymbol x y
    (_, SumColoredMana{}) -> SumColoredMana colorToManaSymbol x y

instance ColorToManaSymbol color => Monoid (ColoredMana v color) where
  mempty = ColoredMana' colorToManaSymbol 0

instance ColorToManaSymbol color => Num (ColoredMana 'NoVar color) where
  (+) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' colorToManaSymbol $ x + y
  (-) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' colorToManaSymbol $ x - y
  (*) (ColoredMana' _ x) (ColoredMana' _ y) = ColoredMana' colorToManaSymbol $ x * y
  abs (ColoredMana' _ x) = ColoredMana' colorToManaSymbol $ abs x
  signum (ColoredMana' _ x) = ColoredMana' colorToManaSymbol $ signum x
  negate (ColoredMana' _ x) = ColoredMana' colorToManaSymbol $ negate x
  fromInteger = ColoredMana' colorToManaSymbol . fromInteger

instance ColorToManaSymbol c => ForceVars (ColoredMana v c) (ColoredMana 'NoVar c) where
  forceVars = \case
    ColoredMana' _ n -> ColoredMana' colorToManaSymbol n
    VariableColoredMana _ (ReifiedVariable _ n) -> ColoredMana' colorToManaSymbol n
    SumColoredMana _ x y -> forceVars x + forceVars y
