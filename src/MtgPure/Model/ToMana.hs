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

class ToMana (mana :: Type) (snow :: Snow) (mt :: ManaType) | mana -> snow mt where
  toMana :: mana -> Mana snow mt

instance ToMana (Mana snow a) snow a where
  toMana = id

instance ToMana (ColoredMana 'White) 'NonSnow 'MTWhite where
  toMana = WhiteMana

instance ToMana (ColoredMana 'Blue) 'NonSnow 'MTBlue where
  toMana = BlueMana

instance ToMana (ColoredMana 'Black) 'NonSnow 'MTBlack where
  toMana = BlackMana

instance ToMana (ColoredMana 'Red) 'NonSnow 'MTRed where
  toMana = RedMana

instance ToMana (ColoredMana 'Green) 'NonSnow 'MTGreen where
  toMana = GreenMana

instance ToMana ColorlessMana 'NonSnow 'MTColorless where
  toMana = ColorlessMana

instance ToMana GenericMana 'NonSnow 'MTGeneric where
  toMana = GenericMana

instance ToMana Int 'NonSnow 'MTGeneric where
  toMana = toMana . GenericMana'

instance ToMana (ManaSymbol 'MTWhite, Int) 'NonSnow 'MTWhite where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTBlue, Int) 'NonSnow 'MTBlue where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTBlack, Int) 'NonSnow 'MTBlack where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTRed, Int) 'NonSnow 'MTRed where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTGreen, Int) 'NonSnow 'MTGreen where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTColorless, Int) 'NonSnow 'MTColorless where
  toMana (~C, n) = toMana $ ColorlessMana' n

instance ToMana (ManaSymbol 'MTSnow, Int) 'Snow 'MTGeneric where
  toMana (~S, n) = GenericMana $ GenericMana' n

instance ToMana (ManaSymbol 'MTWhite) 'NonSnow 'MTWhite where
  toMana ~W = toMana (W, 1 :: Int)

instance ToMana (ManaSymbol 'MTBlue) 'NonSnow 'MTBlue where
  toMana ~U = toMana (U, 1 :: Int)

instance ToMana (ManaSymbol 'MTBlack) 'NonSnow 'MTBlack where
  toMana ~B = toMana (B, 1 :: Int)

instance ToMana (ManaSymbol 'MTRed) 'NonSnow 'MTRed where
  toMana ~R = toMana (R, 1 :: Int)

instance ToMana (ManaSymbol 'MTGreen) 'NonSnow 'MTGreen where
  toMana ~G = toMana (G, 1 :: Int)

instance ToMana (ManaSymbol 'MTColorless) 'NonSnow 'MTColorless where
  toMana ~C = toMana (C, 1 :: Int)

instance ToMana (ManaSymbol 'MTSnow) 'Snow 'MTGeneric where
  toMana ~S = toMana (S, 1 :: Int)
