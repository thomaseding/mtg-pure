{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ToMana (
  ToMana (..),
) where

import safe Data.Kind (Type)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColoredMana (ColoredMana (..))
import safe MtgPure.Model.ColorlessMana (ColorlessMana (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.Mana (Mana (..), Snow (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.Variable (Var (..))

class ToMana (var :: Var) (mana :: Type) (snow :: Snow) (mt :: ManaType) | mana -> snow mt where
  toMana :: mana -> Mana var snow mt

instance ToMana var (Mana var snow a) snow a where
  toMana = id

instance ToMana var (ColoredMana var 'White) 'NonSnow 'MTWhite where
  toMana = WhiteMana

instance ToMana var (ColoredMana var 'Blue) 'NonSnow 'MTBlue where
  toMana = BlueMana

instance ToMana var (ColoredMana var 'Black) 'NonSnow 'MTBlack where
  toMana = BlackMana

instance ToMana var (ColoredMana var 'Red) 'NonSnow 'MTRed where
  toMana = RedMana

instance ToMana var (ColoredMana var 'Green) 'NonSnow 'MTGreen where
  toMana = GreenMana

instance ToMana var (ColorlessMana var) 'NonSnow 'MTColorless where
  toMana = ColorlessMana

instance ToMana var (GenericMana var) 'NonSnow 'MTGeneric where
  toMana = GenericMana

instance ToMana var Int 'NonSnow 'MTGeneric where
  toMana = toMana . GenericMana' @var

instance ToMana var (ManaSymbol 'MTWhite, Int) 'NonSnow 'MTWhite where
  toMana = toMana . uncurry (ColoredMana' @var)

instance ToMana var (ManaSymbol 'MTBlue, Int) 'NonSnow 'MTBlue where
  toMana = toMana . uncurry (ColoredMana' @var)

instance ToMana var (ManaSymbol 'MTBlack, Int) 'NonSnow 'MTBlack where
  toMana = toMana . uncurry (ColoredMana' @var)

instance ToMana var (ManaSymbol 'MTRed, Int) 'NonSnow 'MTRed where
  toMana = toMana . uncurry (ColoredMana' @var)

instance ToMana var (ManaSymbol 'MTGreen, Int) 'NonSnow 'MTGreen where
  toMana = toMana . uncurry (ColoredMana' @var)

instance ToMana var (ManaSymbol 'MTColorless, Int) 'NonSnow 'MTColorless where
  toMana (~C, n) = toMana (ColorlessMana' @var n)

instance ToMana var (ManaSymbol 'MTSnow, Int) 'Snow 'MTGeneric where
  toMana (~S, n) = GenericMana $ GenericMana' n

instance ToMana var (ManaSymbol 'MTWhite) 'NonSnow 'MTWhite where
  toMana ~W = toMana (W, 1 :: Int)

instance ToMana var (ManaSymbol 'MTBlue) 'NonSnow 'MTBlue where
  toMana ~U = toMana (U, 1 :: Int)

instance ToMana var (ManaSymbol 'MTBlack) 'NonSnow 'MTBlack where
  toMana ~B = toMana (B, 1 :: Int)

instance ToMana var (ManaSymbol 'MTRed) 'NonSnow 'MTRed where
  toMana ~R = toMana (R, 1 :: Int)

instance ToMana var (ManaSymbol 'MTGreen) 'NonSnow 'MTGreen where
  toMana ~G = toMana (G, 1 :: Int)

instance ToMana var (ManaSymbol 'MTColorless) 'NonSnow 'MTColorless where
  toMana ~C = toMana (C, 1 :: Int)

instance ToMana var (ManaSymbol 'MTSnow) 'Snow 'MTGeneric where
  toMana ~S = toMana (S, 1 :: Int)
