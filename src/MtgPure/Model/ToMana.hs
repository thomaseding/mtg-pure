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

module MtgPure.Model.ToMana
  ( ToMana (..),
  )
where

import MtgPure.Model.Color (Color (..))
import MtgPure.Model.ColoredMana (ColoredMana (..))
import MtgPure.Model.ColorlessMana (ColorlessMana (..))
import MtgPure.Model.GenericMana (GenericMana (..))
import MtgPure.Model.Mana (Mana (..))
import MtgPure.Model.ManaSymbol (ManaSymbol (..))
import MtgPure.Model.ManaType (ManaType (..))

class ToMana a b | a -> b where
  toMana :: a -> Mana b

instance ToMana (Mana a) a where
  toMana = id

instance ToMana (ColoredMana 'White) 'MTWhite where
  toMana = WhiteMana

instance ToMana (ColoredMana 'Blue) 'MTBlue where
  toMana = BlueMana

instance ToMana (ColoredMana 'Black) 'MTBlack where
  toMana = BlackMana

instance ToMana (ColoredMana 'Red) 'MTRed where
  toMana = RedMana

instance ToMana (ColoredMana 'Green) 'MTGreen where
  toMana = GreenMana

instance ToMana ColorlessMana 'MTColorless where
  toMana = ColorlessMana

instance ToMana GenericMana 'MTGeneric where
  toMana = GenericMana

instance ToMana Int 'MTGeneric where
  toMana = toMana . GenericMana'

instance ToMana (ManaSymbol 'MTWhite, Int) 'MTWhite where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTBlue, Int) 'MTBlue where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTBlack, Int) 'MTBlack where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTRed, Int) 'MTRed where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTGreen, Int) 'MTGreen where
  toMana = toMana . uncurry ColoredMana'

instance ToMana (ManaSymbol 'MTColorless, Int) 'MTColorless where
  toMana (~C, n) = toMana $ ColorlessMana' n

instance ToMana (ManaSymbol 'MTWhite) 'MTWhite where
  toMana ~W = toMana (W, 1 :: Int)

instance ToMana (ManaSymbol 'MTBlue) 'MTBlue where
  toMana ~U = toMana (U, 1 :: Int)

instance ToMana (ManaSymbol 'MTBlack) 'MTBlack where
  toMana ~B = toMana (B, 1 :: Int)

instance ToMana (ManaSymbol 'MTRed) 'MTRed where
  toMana ~R = toMana (R, 1 :: Int)

instance ToMana (ManaSymbol 'MTGreen) 'MTGreen where
  toMana ~G = toMana (G, 1 :: Int)

instance ToMana (ManaSymbol 'MTColorless) 'MTColorless where
  toMana ~C = toMana (C, 1 :: Int)
