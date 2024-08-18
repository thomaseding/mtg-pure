{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColorsLike (
  ColorsLike (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

class ColorsLike a where
  toColors :: a -> Colors

instance ColorsLike Colors where
  toColors :: Colors -> Colors
  toColors = id

instance ColorsLike () where
  toColors :: () -> Colors
  toColors ~() = mempty

instance ColorsLike Color where
  toColors :: Color -> Colors
  toColors = \case
    White -> toColors W
    Blue -> toColors U
    Black -> toColors B
    Red -> toColors R
    Green -> toColors G

instance ColorsLike (ManaSymbol 'TyW) where
  toColors :: ManaSymbol 'TyW -> Colors
  toColors ~W = Colors (Just W) Nothing Nothing Nothing Nothing

instance ColorsLike (ManaSymbol 'TyU) where
  toColors :: ManaSymbol 'TyU -> Colors
  toColors ~U = Colors Nothing (Just U) Nothing Nothing Nothing

instance ColorsLike (ManaSymbol 'TyB) where
  toColors :: ManaSymbol 'TyB -> Colors
  toColors ~B = Colors Nothing Nothing (Just B) Nothing Nothing

instance ColorsLike (ManaSymbol 'TyR) where
  toColors :: ManaSymbol 'TyR -> Colors
  toColors ~R = Colors Nothing Nothing Nothing (Just R) Nothing

instance ColorsLike (ManaSymbol 'TyG) where
  toColors :: ManaSymbol 'TyG -> Colors
  toColors ~G = Colors Nothing Nothing Nothing Nothing (Just G)

instance (Inst2 ColorsLike a b) => ColorsLike (a, b) where
  toColors :: (Inst2 ColorsLike a b) => (a, b) -> Colors
  toColors (a, b) = toColors a <> toColors b

instance (Inst3 ColorsLike a b c) => ColorsLike (a, b, c) where
  toColors :: (Inst3 ColorsLike a b c) => (a, b, c) -> Colors
  toColors (a, b, c) = toColors a <> toColors b <> toColors c

instance (Inst4 ColorsLike a b c d) => ColorsLike (a, b, c, d) where
  toColors :: (Inst4 ColorsLike a b c d) => (a, b, c, d) -> Colors
  toColors (a, b, c, d) = toColors a <> toColors b <> toColors c <> toColors d

instance (Inst5 ColorsLike a b c d e) => ColorsLike (a, b, c, d, e) where
  toColors :: (Inst5 ColorsLike a b c d e) => (a, b, c, d, e) -> Colors
  toColors (a, b, c, d, e) =
    toColors a <> toColors b <> toColors c <> toColors d <> toColors e
